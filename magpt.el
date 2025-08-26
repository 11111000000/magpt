;;; magpt.el --- Generate commit messages from staged diff via gptel + Magit  -*- lexical-binding: t; -*-

;; Author: Your Name <you@example.com>
;; URL: https://github.com/yourname/magpt
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (gptel "0.9"))
;; Keywords: tools, vc, git, ai

;;; Commentary:
;;
;; magpt — это пакет для Emacs, который помогает писать сообщения коммитов
;; на основе диффа застейдженных изменений, используя gptel (LLM-провайдеры)
;; и по возможности интегрируется с Magit.
;;
;; Основные возможности:
;; - Получить diff всех застейдженных изменений текущего Git-проекта.
;; - Сформировать подсказку (prompt) = =magpt-commit-prompt' + diff.
;; - Отправить запрос в LLM через =gptel-request' асинхронно.
;; - Вставить результат в буфер сообщения коммита (git-commit-mode) либо
;;   показать в отдельном буфере.
;;
;; Архитектурные заметки:
;; - Внутренние функции (с префиксом magpt--) стремятся быть чистыми: они
;;   принимают аргументы и возвращают значения без побочных эффектов.
;; - Побочные эффекты (взаимодействие с буферами/окнами/сообщениями) — в
;;   интерактивных командах и коллбэках.
;; - Magit — необязательная зависимость; при наличии — используется для
;;   определения корня репозитория. Есть варианты с vc/project.el.
;;
;; Использование:
;; - Вызовите команду:
;;     M-x magpt-generate-commit-message
;; - Настройте переменные в группе =magpt' при необходимости.

;;; Code:

(require 'gptel)
(require 'vc)
(require 'project)
(eval-when-compile (require 'subr-x))
(require 'magit nil t) ;; опционально

(defgroup magpt nil
  "Генерация сообщений коммитов из staged diff через gptel."
  :group 'tools
  :group 'vc
  :prefix "magpt-")

(defcustom magpt-model "gpt-4.1"
  "Имя модели LLM для gptel или nil.
Если nil — используется текущая модель/провайдер по умолчанию gptel.
Например: \"gpt-4o-mini\" или любая доступная модели в вашей конфигурации gptel."
  :type '(choice (const :tag "Использовать модель по умолчанию gptel" nil)
                 (string :tag "Явно указать модель"))
  :group 'magpt)

(defcustom magpt-commit-prompt
  "You are an assistant that writes high-quality Git commit messages.
Requirements:
- Use Conventional Commits types when applicable (feat, fix, docs, refactor, test, chore, perf, build, ci).
- First line: concise summary <= 72 chars.
- Optional body: wrap at ~72 chars per line; explain motivation, context, and impact.
- Use imperative mood; do not include ticket/issue references unless present in diff.
- If the diff is empty or unclear, say 'chore: update' with a brief rationale.
Provide the final commit message only, no extra commentary."
  "Шаблонная подсказка, дополняемая диффом застейдженных изменений.
Итоговый prompt: это эта строка + разделитель + сам diff."
  :type 'string
  :group 'magpt)

(defcustom magpt-max-diff-bytes 200000
  "Максимальный размер diff в байтах для включения в запрос.
Если nil — не ограничивать. Если diff превышает лимит, он будет усечён,
а в подсказку будет добавлено уведомление о сокращении."
  :type '(choice (const :tag "Без ограничения" nil)
                 (integer :tag "Макс. байт"))
  :group 'magpt)

(defcustom magpt-insert-into-commit-buffer t
  "Если t, то результат будет вставлен в буфер сообщения коммита (git-commit-mode), если он доступен.
Иначе результат покажется в отдельном буфере *magpt-commit*."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-project-root-strategy 'prefer-magit
  "Стратегия определения корня проекта (Git) для получения diff.
- prefer-magit: сперва Magit, затем VC, затем project.el, затем default-directory (с проверкой).
- prefer-vc: сперва VC, затем Magit, затем project.el, затем default-directory (с проверкой).
- prefer-project: сперва project.el, затем Magit, затем VC, затем default-directory (с проверкой)."
  :type '(choice
          (const :tag "С приоритетом Magit" prefer-magit)
          (const :tag "С приоритетом VC" prefer-vc)
          (const :tag "С приоритетом project.el" prefer-project))
  :group 'magpt)

(defcustom magpt-diff-args '("--staged" "--no-color")
  "Дополнительные аргументы для команды git diff.
По умолчанию используется staged diff без цвета."
  :type '(repeat string)
  :group 'magpt)

;;;; Вспомогательные внутренние функции (чистые, без побочных эффектов)

(defun magpt--executable-git ()
  "Вернуть путь к исполняемому файлу git или nil."
  (executable-find "git"))

(defun magpt--process-git (dir &rest args)
  "Выполнить команду git с ARGS в каталоге DIR.
Вернуть cons (EXIT-CODE . STRING-OUTPUT). Не сигнализирует ошибки сама по себе."
  (let ((default-directory (file-name-as-directory (or dir default-directory))))
    (with-temp-buffer
      (let ((exit (apply #'process-file (or (magpt--executable-git) "git")
                         nil t nil args))
            (out  (buffer-string)))
        (cons exit (if (string-suffix-p "\n" out) (substring out 0 -1) out))))))

(defun magpt--git (dir &rest args)
  "Выполнить git ARGS в DIR и вернуть строку вывода.
Если команда завершилась с ненулевым кодом, сигнализировать =user-error'
с включением текста ошибки."
  (pcase (apply #'magpt--process-git dir args)
    (`(,exit . ,out)
     (if (zerop exit)
         out
       (user-error "Git ошибка (%s): %s" exit out)))))

(defun magpt--git-root-from (dir)
  "Попытаться получить корень Git-репозитория для каталога DIR.
Вернуть строку пути или nil, если не удалось."
  (when (and (magpt--executable-git) (file-directory-p dir))
    (let ((res (magpt--process-git dir "rev-parse" "--show-toplevel")))
      (when (eq (car res) 0)
        (file-name-as-directory (cdr res))))))

(defun magpt--try-root-from-magit ()
  "Вернуть корень репозитория, используя Magit, либо nil."
  (when (and (featurep 'magit) (fboundp 'magit-toplevel))
    (ignore-errors
      (let ((root (magit-toplevel)))
        (when (and (stringp root) (file-directory-p root))
          (file-name-as-directory root))))))

(defun magpt--try-root-from-vc ()
  "Вернуть корень репозитория, используя VC, либо nil."
  (let ((root (ignore-errors (vc-root-dir))))
    (when (and (stringp root) (file-directory-p root))
      (file-name-as-directory root))))

(defun magpt--try-root-from-project ()
  "Вернуть корень проекта, используя project.el, либо nil."
  (let* ((proj (ignore-errors (project-current)))
         (root (when proj (ignore-errors (project-root proj)))))
    (when (and (stringp root) (file-directory-p root))
      (file-name-as-directory root))))

(defun magpt--project-root ()
  "Определить корень Git-проекта согласно =magpt-project-root-strategy'.
Сигнализировать ошибку, если репозиторий не найден."
  (let* ((candidates
          (pcase magpt-project-root-strategy
            ('prefer-magit
             (list #'magpt--try-root-from-magit
                   #'magpt--try-root-from-vc
                   #'magpt--try-root-from-project))
            ('prefer-vc
             (list #'magpt--try-root-from-vc
                   #'magpt--try-root-from-magit
                   #'magpt--try-root-from-project))
            ('prefer-project
             (list #'magpt--try-root-from-project
                   #'magpt--try-root-from-magit
                   #'magpt--try-root-from-vc))
            (_ (list #'magpt--try-root-from-magit
                     #'magpt--try-root-from-vc
                     #'magpt--try-root-from-project))))
         (root (seq-some (lambda (f) (funcall f)) candidates)))
    (setq root (or root (magpt--git-root-from default-directory)))
    (setq root (or root (when (file-directory-p default-directory)
                          (let ((probe (magpt--git-root-from default-directory)))
                            probe))))
    (unless root
      (user-error "Не найден Git-репозиторий для текущего каталога"))
    root))

(defun magpt--staged-diff (root)
  "Вернуть строку diff для застейдженных изменений в ROOT.
Использует =magpt-diff-args' поверх =git diff'."
  (apply #'magpt--git root "diff" (or magpt-diff-args '("--staged" "--no-color"))))

(defun magpt--truncate-to-bytes (s max-bytes)
  "Вернуть максимально длинный префикс S, чей размер в UTF-8 не превышает MAX-BYTES.
Используется бинарный поиск по количеству символов для сохранения корректной границы кодировки."
  (let* ((len (length s)))
    (if (or (null max-bytes) (<= (string-bytes s) max-bytes))
        s
      (let ((lo 0)
            (hi len)
            (best 0))
        (while (< lo hi)
          (let* ((mid (/ (+ lo hi 1) 2))
                 (candidate (substring s 0 mid))
                 (bytes (string-bytes candidate)))
            (if (<= bytes max-bytes)
                (setq best mid
                      lo mid)
              (setq hi (1- mid)))))
        (substring s 0 best)))))

(defun magpt--maybe-truncate (s max-bytes)
  "Возможно усечь строку S до MAX-BYTES байт.
Вернёт cons (TRUNCATED-OR-ORIGINAL . TRUNCATEDP)."
  (if (or (null max-bytes) (<= (string-bytes s) max-bytes))
      (cons s nil)
    (let ((tstr (magpt--truncate-to-bytes s max-bytes)))
      (cons tstr t))))

(defun magpt--build-commit-prompt (template diff &optional truncatedp)
  "Собрать окончательный prompt для LLM.
TEMPLATE — шаблон (строка), DIFF — строка diff.
Если TRUNCATEDP не-nil — добавить пометку об усечении diff."
  (concat
   (string-trim-right (or template ""))
   "\n\n--- BEGIN DIFF ---\n"
   diff
   (when truncatedp "\n\n[diff truncated due to size limit]")
   "\n--- END DIFF ---\n"))

(defun magpt--find-commit-buffer ()
  "Найти активный буфер с major-mode =git-commit-mode', если он есть."
  (catch 'found
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (derived-mode-p 'git-commit-mode)
            (throw 'found buf)))))
    nil))

(defun magpt--insert-into-commit-buffer (text)
  "Вставить TEXT в буфер сообщения коммита (git-commit-mode), спросив подтверждение при необходимости.
Возвращает t, если вставка произведена; nil иначе."
  (let ((target (or (and (derived-mode-p 'git-commit-mode) (current-buffer))
                    (magpt--find-commit-buffer))))
    (when target
      (with-current-buffer target
        (let* ((raw (buffer-substring-no-properties (point-min) (point-max)))
               (content (string-trim (replace-regexp-in-string "^#.*$" "" raw))))
          (when (or (string-empty-p content)
                    (y-or-n-p "Очистить буфер коммита и вставить сгенерированное сообщение? "))
            (erase-buffer)
            (insert (string-trim-right text) "\n")
            (goto-char (point-min))
            (message "magpt: сообщение коммита вставлено в %s" (buffer-name target))
            t))))))

(defun magpt--show-in-output-buffer (text)
  "Показать TEXT в буфере *magpt-commit* и скопировать в kill-ring."
  (let ((buf (get-buffer-create "*magpt-commit*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-trim-right text) "\n")
        (goto-char (point-min))
        (text-mode)
        (setq buffer-read-only t)))
    (pop-to-buffer buf)
    (kill-new (string-trim-right text))
    (message "magpt: результат скопирован в kill-ring и показан в *magpt-commit*")))

;;;; Публичная команда

;;;###autoload
(defun magpt-generate-commit-message ()
  "Сгенерировать сообщение для коммита из staged diff текущего проекта с помощью gptel.
Поведение:
- Если =magpt-insert-into-commit-buffer' = t и доступен буфер git-commit-mode,
  результат вставляется в этот буфер (с подтверждением при наличии содержимого).
- Иначе результат открывается в отдельном буфере /magpt-commit/ и копируется в kill-ring."
  (interactive)
  (unless (magpt--executable-git)
    (user-error "Не найден исполняемый файл 'git' в PATH"))
  (let* ((root (magpt--project-root))
         (diff (magpt--staged-diff root)))
    (when (string-empty-p (string-trim diff))
      (user-error "Нет застейдженных изменений (git add ...)"))
    (let* ((trunc (magpt--maybe-truncate diff magpt-max-diff-bytes))
           (diff (car trunc))
           (truncatedp (cdr trunc))
           (prompt (magpt--build-commit-prompt magpt-commit-prompt diff truncatedp))
           (callback
            (lambda (response info)
              (condition-case err
                  (let ((errstr (plist-get info :error)))
                    (if errstr
                        (message "magpt/gptel ошибка: %s" errstr)
                      (let ((text (string-trim (or response ""))))
                        (if (string-empty-p text)
                            (message "magpt: пустой ответ от модели")
                          (if (and magpt-insert-into-commit-buffer
                                   (magpt--insert-into-commit-buffer text))
                              (message "magpt: сообщение вставлено")
                            (magpt--show-in-output-buffer text))))))

                (error
                 (message "magpt: ошибка в коллбэке: %s" (error-message-string err)))))))
      (message "magpt: запрашиваю LLM для генерации сообщения коммита...")
      (condition-case err
          (let ((args (append (when magpt-model (list :model magpt-model))
                              (list :callback callback))))
            (apply #'gptel-request prompt args))
        (error
         (message "magpt: ошибка при вызове gptel: %s" (error-message-string err)))))))

(provide 'magpt)

;;; magpt.el ends here
