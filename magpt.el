;;; magpt.el --- Generate commit messages from staged diff via gptel + Magit  -*- lexical-binding: t; -*-

;; Author: Peter <11111000000@email.com>
;; URL: https://github.com/11111000000/magpt
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

(defun magpt--commit-buffer-p (&optional buf)
  "Вернуть t, если BUF выглядит как буфер сообщения коммита.
Признаки:
- major-mode наследуется от git-commit-mode; или
- это файл COMMIT_EDITMSG; или
- включён with-editor-mode (буфер редактирования сообщения)."
  (with-current-buffer (or buf (current-buffer))
    (or (derived-mode-p 'git-commit-mode)
        (and buffer-file-name
             (string-equal (file-name-nondirectory buffer-file-name)
                           "COMMIT_EDITMSG"))
        (bound-and-true-p with-editor-mode))))

(defun magpt--find-commit-buffer ()
  "Найти активный буфер сообщения коммита, если он есть."
  (catch 'found
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (magpt--commit-buffer-p buf))
        (throw 'found buf)))
    nil))

(defcustom magpt-commit-overlay-text "Message generation..."
  "Текст оверлея, показываемого в буфере коммита пока идёт генерация."
  :type 'string
  :group 'magpt)

(defface magpt-commit-overlay-face
  '((t :inherit font-lock-comment-delimiter-face :weight bold))
  "Лицо для оверлея генерации сообщения коммита."
  :group 'magpt)

(defvar-local magpt--commit-overlay nil
  "Оверлей, показываемый в буфере коммита на время генерации сообщения.")

(defun magpt--show-commit-overlay (buf)
  "Показать оверлей в буфере BUF, сигнализирующий о генерации сообщения."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (unless (overlayp magpt--commit-overlay)
          (setq magpt--commit-overlay (make-overlay (point-min) (point-min) buf t t)))
        (overlay-put magpt--commit-overlay 'before-string
                     (propertize (concat magpt-commit-overlay-text "\n")
                                 'face 'magpt-commit-overlay-face))))))

(defun magpt--remove-commit-overlay (buf)
  "Удалить оверлей в буфере BUF, если он есть."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (overlayp magpt--commit-overlay)
        (delete-overlay magpt--commit-overlay)
        (setq magpt--commit-overlay nil)))))

(defun magpt--insert-into-commit-buffer (text)
  "Вставить TEXT в буфер сообщения коммита, спросив подтверждение при необходимости.
Сохраняет строки-комментарии (начинающиеся с #) внизу буфера. Вставляет
сгенерированное сообщение в начало. Возвращает t, если вставка произведена; nil иначе."
  (let ((target (or (and (magpt--commit-buffer-p) (current-buffer))
                    (magpt--find-commit-buffer))))
    (when target
      (with-current-buffer target
        (let* ((comment-pos (save-excursion
                              (goto-char (point-min))
                              (when (re-search-forward "^#.*$" nil t)
                                (match-beginning 0))))
               (msg-end (or comment-pos (point-max)))
               (existing (string-trim (buffer-substring-no-properties (point-min) msg-end))))
          (when (or (string-empty-p existing)
                    (y-or-n-p "Заменить текущее сообщение и вставить сгенерированное? "))
            (let ((inhibit-read-only t))
              ;; Удаляем только текущее сообщение (до комментариев), комментарии сохраняем.
              (delete-region (point-min) msg-end)
              (goto-char (point-min))
              (insert (string-trim-right text) "\n")
              (goto-char (point-min)))
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
          (let ((gptel-model (or magpt-model gptel-model)))
            (gptel-request prompt :callback callback))
        (error
         (message "magpt: ошибка при вызове gptel: %s" (error-message-string err)))))))

(defun magpt--insert-into-commit-buffer-target (buf text)
  "Вставить TEXT в заданный буфер BUF сообщения коммита.
Сохраняет строки-комментарии (начинающиеся с #) внизу буфера. Вставляет
сгенерированное сообщение в начало. Возвращает t, если вставка произведена; nil иначе."
  (when (and (buffer-live-p buf))
    (with-current-buffer buf
      (when (magpt--commit-buffer-p)
        (let* ((comment-pos (save-excursion
                              (goto-char (point-min))
                              (when (re-search-forward "^#.*$" nil t)
                                (match-beginning 0))))
               (msg-end (or comment-pos (point-max)))
               (existing (string-trim (buffer-substring-no-properties (point-min) msg-end))))
          (when (or (string-empty-p existing)
                    (y-or-n-p "Заменить текущее сообщение и вставить сгенерированное? "))
            (let ((inhibit-read-only t))
              ;; Удаляем только текущее сообщение (до комментариев), комментарии сохраняем.
              (delete-region (point-min) msg-end)
              (goto-char (point-min))
              (insert (string-trim-right text) "\n")
              (goto-char (point-min)))
            (message "magpt: сообщение коммита вставлено в %s" (buffer-name buf))
            t))))))

(defun magpt--commit-callback (response info)
  "Коллбэк для gptel. Вставляет ответ в буфер коммита.
Сначала использует буфер из :context, а если его нет или он недоступен —
пытается найти любой активный буфер коммита. Если буфера нет,
выводит результат в *Messages*."
  (condition-case err
      (let* ((errstr (plist-get info :error))
             (commit-buf (plist-get info :context))
             (target (or (and (buffer-live-p commit-buf)
                              (magpt--commit-buffer-p commit-buf)
                              commit-buf)
                         (magpt--find-commit-buffer))))
        (if errstr
            (progn
              (when (buffer-live-p commit-buf)
                (magpt--remove-commit-overlay commit-buf))
              (message "magpt/gptel ошибка: %s" errstr))
          (let ((text (string-trim (or response ""))))
            (cond
             ((string-empty-p text)
              (when (buffer-live-p target)
                (magpt--remove-commit-overlay target))
              (message "magpt: пустой ответ от модели"))
             (target
              ;; Убираем оверлей перед вставкой.
              (magpt--remove-commit-overlay target)
              (if (magpt--insert-into-commit-buffer-target target text)
                  (message "magpt: сообщение вставлено в буфер коммита")
                (message "magpt: вставка отменена пользователем")))
             (t
              (when (buffer-live-p commit-buf)
                (magpt--remove-commit-overlay commit-buf))
              (message "magpt: буфер коммита недоступен; сгенерированное сообщение:\n%s" text))))))
    (error
     (message "magpt: ошибка в коллбэке: %s" (error-message-string err)))))

;;;###autoload
(defun magpt-commit-staged ()
  "Начать коммит Magit и вставить сгенерированное сообщение для застейдженных изменений.
Откроет буфер сообщения коммита, подставит текст, а завершить
коммит пользователь сможет стандартной командой C-c C-c. Сам коммит эта команда
не выполняет.

Требует установленного Magit."
  (interactive)
  (unless (magpt--executable-git)
    (user-error "Не найден исполняемый файл 'git' в PATH"))
  (unless (and (require 'magit nil t) (fboundp 'magit-commit-create))
    (user-error "Для magpt-commit-staged требуется Magit"))
  (let* ((root (magpt--project-root))
         (diff (magpt--staged-diff root)))
    (when (string-empty-p (string-trim diff))
      (user-error "Нет застейдженных изменений (git add ...)"))
    (let* ((trunc (magpt--maybe-truncate diff magpt-max-diff-bytes))
           (diff (car trunc))
           (truncatedp (cdr trunc))
           (prompt (magpt--build-commit-prompt magpt-commit-prompt diff truncatedp))
           (target-buf nil))
      ;; Открыть буфер сообщения коммита и попытаться дождаться его появления.
      (condition-case err
          (progn
            (magit-commit-create nil)
            ;; Небольшое ожидание, чтобы буфер гарантированно появился и активировался.
            (let ((tries 0))
              (while (and (null target-buf) (< tries 50))
                (setq target-buf (or (and (magpt--commit-buffer-p) (current-buffer))
                                     (magpt--find-commit-buffer)))
                (unless target-buf (sit-for 0.01))
                (setq tries (1+ tries)))))
        (error
         (user-error "Не удалось открыть буфер коммита Magit: %s" (error-message-string err))))
      ;; Показать оверлей «Message generation…» в буфере коммита на время запроса.
      (when target-buf
        (magpt--show-commit-overlay target-buf))
      (message "magpt: запрашиваю LLM для генерации сообщения коммита...")
      (condition-case err
          (let ((gptel-model (or magpt-model gptel-model)))
            (gptel-request prompt :context target-buf :callback #'magpt--commit-callback))
        (error
         (when target-buf
           (magpt--remove-commit-overlay target-buf))
         (message "magpt: ошибка при вызове gptel: %s" (error-message-string err)))))))

(provide 'magpt)

;;; magpt.el ends here
