;;; magpt.el --- MaGPT: Git/Magit AI assistant via gptel  -*- lexical-binding: t; -*-

;; Author: Peter <11111000000@email.com>
;; URL: https://github.com/11111000000/magpt
;; Version: 1.7.0
;; Package-Requires: ((emacs "28.1") (gptel "0.9"))
;; Keywords: tools, vc, git, ai

;;; Commentary:
;;
;; MaGPT (magpt.el) is an Emacs package that augments your Git/Magit workflow
;; with AI-powered assistance via gptel. It started as an AI commit message
;; generator and is evolving towards a safe, task-oriented assistant that can:
;; - observe repository state and explain it,
;; - suggest next actions and commit structure,
;; - assist with messages, summaries, and release notes,
;; - provide reversible, preview-first help for complex flows (e.g., conflicts).
;;
;; Design principles:
;; - Provider-agnostic: inherit configuration from gptel; no hardcoding of models.
;; - Safety first: explicit confirmation before sending data; minimal context; masking-ready.
;; - Reversibility: never mutate without preview and confirmation; no hidden Git side-effects.
;; - Clear UX: async and non-blocking; overlays show progress; errors clean up gracefully.
;; - Extensible core: “Task” registry (context → prompt → request → render/apply) for future features.
;;
;; This file is organized into well-delimited sections to support future splitting
;; into multiple modules (core, commit, tasks, ui, etc.) without changing behavior.
;; All public entry points and user-facing behavior remain backward-compatible.

;;; Code:

;; Ensure local requires work when loading this file directly via load-file.
(eval-and-compile
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (when (and dir (file-directory-p dir))
      (add-to-list 'load-path dir))))

;;;; Section: Dependencies and forward declarations
;;
;; We keep core deps lightweight. Magit/transient are optional. gptel is required for requests.

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'json)
(require 'vc)
(require 'project)
(require 'gptel)
(require 'magit nil t)     ;; Optional; used when available
(require 'transient nil t) ;; Optional; used when available
(require 'magpt-ui-preview nil t)
(require 'magpt-magit-overview nil t)
(require 'magpt-history nil t) ;; ensure history API is accessible
(require 'magpt-tasks-core nil t)
(require 'magpt-tasks-assist nil t)
(require 'magpt-tasks-recommend nil t)
(require 'magpt-tasks-resolve nil t)
(require 'magpt-commit nil t)
(require 'magpt-transient nil t)
(require 'magpt-gpt nil t)
(require 'magpt-git nil t)

(declare-function magit-toplevel "ext:magit")
(declare-function magit-commit-create "ext:magit")
(declare-function transient-quit-all "ext:transient")
(defvar transient--prefix) ;; from transient
(declare-function magpt--recent-git-output-get "magpt-git" (dir))
(declare-function magpt--git-apply-temp "magpt-git" (dir patch &rest args))
(declare-function magpt--git-apply-check-temp "magpt-git" (dir patch &rest args))

;;;; Section: Feature flags and “public” groups
;;
;; All user options live under the magpt group. The module is provider-agnostic and safe by default.

(defgroup magpt nil
  "MaGPT: Git/Magit AI assistant via gptel (commit messages and assist tasks)."
  :group 'tools
  :group 'vc
  :prefix "magpt-")

;;;; Section: Customization — core options
;;
;; These options control models, prompts, size limits, UX, and repo discovery.

(defcustom magpt-model nil
  "LLM model name for gptel. If nil, uses gptel’s currently-selected default."
  :type '(choice (const :tag "Use gptel default model" nil)
                 (string :tag "Explicit model name"))
  :group 'magpt)

(defcustom magpt-info-language "English"
  "Preferred natural language for informative content (summaries, rationales) in assist tasks.
Note: This does not translate Emacs UI; it only nudges the model via prompts."
  :type 'string
  :group 'magpt)

(defcustom magpt-commit-language nil
  "Preferred language for generated commit messages. If nil, no preference."
  :type '(choice (const :tag "No preference" nil)
                 (string :tag "Language"))
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
  "Prompt template for commit message generation. The diff is appended."
  :type 'string
  :group 'magpt)

(defcustom magpt-max-diff-bytes 200000
  "Maximum UTF-8 byte size for the diff sent to the model.
If nil, no limit. When truncated, boundaries respect UTF-8 and a note is added."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Max bytes"))
  :group 'magpt)

(defcustom magpt-insert-into-commit-buffer t
  "If non-nil, insert generated commit messages into the commit buffer when available.
Otherwise show results in a separate read-only buffer."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-project-root-strategy 'prefer-magit
  "Strategy for determining the Git project root:
- prefer-magit    : Magit → VC → project.el → default-directory check
- prefer-vc       : VC → Magit → project.el → default-directory check
- prefer-project  : project.el → Magit → VC → default-directory check"
  :type '(choice
          (const :tag "Prefer Magit" prefer-magit)
          (const :tag "Prefer VC" prefer-vc)
          (const :tag "Prefer project.el" prefer-project))
  :group 'magpt)

(defcustom magpt-diff-args '("--staged" "--no-color")
  "Additional arguments used with git diff when collecting staged changes."
  :type '(repeat string)
  :group 'magpt)

(defcustom magpt-confirm-before-send t
  "If non-nil, ask for confirmation before sending content to the model."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-allow-apply-safe-ops t
  "If non-nil, enable safe apply operations (e.g., stage/unstage whole files) via Magit buttons or commands.
This gates any mutation-producing Apply actions; Phase 2 enables only naturally reversible operations."
  :type 'boolean
  :group 'magpt)

;;;; Section: Project RC (.magptrc) support
;;
;; Per-project overrides with highest priority. The file format is a safe “alist” of (SYMBOL . VALUE).

(defcustom magpt-rc-file-name ".magptrc"
  "Per-project RC file name at the repo root. Overrides user options when present."
  :type 'string
  :group 'magpt)

(defcustom magpt-user-rc-file (expand-file-name "~/.magptrc")
  "Path to user-level magpt RC file. Loaded before project RC; project overrides."
  :type '(choice (const :tag "Disabled" nil)
                 (file :tag "RC file path"))
  :group 'magpt)

(defvar magpt--user-rc-state nil
  "Internal cache of user rc: plist (:path PATH :mtime TIME :data ALIST).")

(defvar magpt--proj-rc-state nil
  "Internal cache of project rc: plist (:path PATH :mtime TIME :data ALIST).")

(defun magpt--locate-project-rc ()
  "Return absolute path to project .magptrc if found; otherwise nil."
  (let ((root (ignore-errors (magpt--project-root))))
    (when root
      (let ((f (expand-file-name magpt-rc-file-name root)))
        (when (file-exists-p f) f)))))

;; Backward-compat alias (older code may expect this name).
(defun magpt--locate-rc ()
  "Return absolute path to project .magptrc if found; otherwise nil."
  (magpt--locate-project-rc))

(defun magpt--locate-user-rc ()
  "Return absolute path to user RC file if configured and exists; otherwise nil."
  (when (and magpt-user-rc-file (stringp magpt-user-rc-file))
    (let ((f (expand-file-name magpt-user-rc-file)))
      (when (file-exists-p f) f))))

(defun magpt--read-rc (file)
  "Read FILE and return an alist of (SYMBOL . VALUE). Ignores arbitrary code; supports quoted list."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((sexp (read (current-buffer))))
          (when (and (listp sexp) (eq (car-safe sexp) 'quote))
            (setq sexp (cadr sexp)))
          (when (listp sexp) sexp)))
    (error
     (magpt--log "rc read error: %s: %s" file (error-message-string err))
     nil)))

(defun magpt--apply-rc (alist)
  "Apply ALIST of (SYMBOL . VALUE) to magpt variables. Highest priority overrides."
  (when (listp alist)
    (dolist (kv alist)
      (pcase kv
        (`(,k . ,v)
         (let* ((sym (cond
                      ((symbolp k) k)
                      ((stringp k) (intern k))
                      (t nil))))
           (when (and sym
                      (string-prefix-p "magpt-" (symbol-name sym))
                      (boundp sym))
             (set sym v))))))))

(defun magpt--maybe-load-user-rc ()
  "Load and apply user RC (~/.magptrc) if present and changed."
  (let ((f (magpt--locate-user-rc)))
    (when f
      (let* ((attr (file-attributes f))
             (mtime (when attr (file-attribute-modification-time attr))))
        (when (or (null magpt--user-rc-state)
                  (not (equal (plist-get magpt--user-rc-state :path) f))
                  (not (equal (plist-get magpt--user-rc-state :mtime) mtime)))
          (let ((alist (magpt--read-rc f)))
            (setq magpt--user-rc-state (list :path f :mtime mtime :data alist))
            (magpt--apply-rc alist)
            (magpt--log "user rc loaded: %s keys=%s"
                        f (mapcar (lambda (kv)
                                    (cond
                                     ((consp kv) (symbol-name (car kv)))
                                     ((symbolp kv) (symbol-name kv))
                                     (t (format "%S" kv))))
                                  (or alist '())))))))))

(defun magpt--maybe-load-project-rc ()
  "Load and apply project .magptrc if present and changed."
  (let ((f (magpt--locate-project-rc)))
    (when f
      (let* ((attr (file-attributes f))
             (mtime (when attr (file-attribute-modification-time attr))))
        (when (or (null magpt--proj-rc-state)
                  (not (equal (plist-get magpt--proj-rc-state :path) f))
                  (not (equal (plist-get magpt--proj-rc-state :mtime) mtime)))
          (let ((alist (magpt--read-rc f)))
            (setq magpt--proj-rc-state (list :path f :mtime mtime :data alist))
            (magpt--apply-rc alist)
            (magpt--log "project rc loaded: %s keys=%s"
                        f (mapcar (lambda (kv)
                                    (cond
                                     ((consp kv) (symbol-name (car kv)))
                                     ((symbolp kv) (symbol-name kv))
                                     (t (format "%S" kv))))
                                  (or alist '())))))))))

(defun magpt--maybe-load-rc ()
  "Load and apply user RC then project RC; project overrides user.
Project RC is always re-applied last to ensure precedence even when only the
user RC changed since the last call."
  ;; Load user-level first to establish defaults.
  (magpt--maybe-load-user-rc)
  ;; Then load project-level, which takes precedence.
  (magpt--maybe-load-project-rc)
  ;; Re-apply project RC data (if available) to enforce precedence consistently.
  (when (and magpt--proj-rc-state (plist-get magpt--proj-rc-state :data))
    (magpt--apply-rc (plist-get magpt--proj-rc-state :data))))

;;;; Section: Logging and diagnostics
;;
;; Diagnostics are lightweight and safe. Use magpt-show-log to open the buffer.

(defcustom magpt-log-enabled t
  "If non-nil, write diagnostic logs to `magpt-log-buffer-name'."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-log-buffer-name "*magpt-log*"
  "Name of the buffer used for diagnostic logs."
  :type 'string
  :group 'magpt)

(defun magpt--log (fmt &rest args)
  "Append a diagnostic line to `magpt-log-buffer-name' and echo minimal info."
  (when magpt-log-enabled
    (let ((buf (get-buffer-create magpt-log-buffer-name)))
      (with-current-buffer buf
        (goto-char (point-max))
        (let* ((ts (format-time-string "%Y-%m-%d %H:%M:%S"))
               (line (condition-case err
                         (apply #'format fmt args)
                       (error
                        (format "LOG-FMT-ERROR: fmt=%S args=%S err=%s"
                                fmt args (error-message-string err))))))
          (insert (format "[%s] %s\n" ts line)))))))

(defun magpt--backtrace-string ()
  "Return current backtrace as a string (best-effort)."
  (condition-case _
      (with-output-to-string (backtrace))
    (error "<no-backtrace>")))

;;;###autoload
(defun magpt-show-log ()
  "Open the magpt diagnostic log buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create magpt-log-buffer-name)))

;;;; Section: i18n helpers (messages for UI and overview)
;;
;; i18n is intentionally minimal and only for user-facing messages, not prompts.

(defun magpt--lang-code ()
  "Return language code symbol based on `magpt-info-language'."
  (let ((l (downcase (or magpt-info-language "english"))))
    (cond
     ((string-match-p "\\`\\(ru\\|рус\\)" l) 'ru)
     ((string-match-p "\\`fr" l) 'fr)
     (t 'en))))

(defconst magpt--i18n-en
  '((confirm-send-full . "magpt: Send staged diff to LLM (%d bytes)? ")
    (confirm-send-trunc . "magpt: Send staged diff to LLM (original %d bytes; sending %d bytes after truncation)? ")
    (request-llm-commit . "magpt: requesting LLM to generate commit message...")
    (result-copied . "magpt: result copied to kill-ring and shown in *magpt-commit*")
    (empty-response . "magpt: empty response from model")
    (insertion-cancelled . "magpt: insertion cancelled by user")
    (inserted-into-commit-buffer . "magpt: commit message inserted into commit buffer")
    (inserted-into-buffer-named . "magpt: commit message inserted into %s")
    (gptel-error . "magpt: error calling gptel: %s")
    (gptel-error2 . "magpt/gptel error: %s")
    (sending-cancelled . "magpt: sending cancelled by user")
    (no-staged-changes . "No staged changes found (git add ...)")
    (replace-current-commit-msg? . "Replace the current commit message and insert the generated one? ")
    (callback-error . "magpt: error in callback: %s")
    ;; Overview/History (keys used by AI Overview)
    (overview-response . "Response:")
    (json-opened . "Opened response in JSON buffer")
    (json-copied . "Response copied to kill-ring")
    ;; Section titles (localized UI)
    (overview-summary . "Summary:")
    (overview-risks . "Risks:")
    (overview-suggestions . "Suggestions:")
    (overview-lint-status . "Lint status:")
    (overview-issues . "Issues:")
    (overview-suggestion . "Suggestion:")
    (overview-name . "Name:")
    (overview-alternatives . "Alternatives:")
    (overview-rationale . "Rationale:")
    (overview-steps . "Steps:")
    ;; Card titles (localized UI)
    (overview-card-commit-lint . "Commit Lint / Fix Suggest")
    (overview-card-branch-name . "Branch Name Suggest")
    (overview-card-resolve-conflict . "Resolve Conflict (here)")
    (overview-card-push-pull . "Push/Pull advice")
    (overview-card-branches . "Branches overview")
    (overview-card-restore-file . "Recover file (how-to)")
    (overview-no-data . "(no data - press [. g] to refresh)")
    (overview-stale . "(status changed - press [. g] to refresh)")
    (patch-opened . "Opened response in patch buffer")))

(defconst magpt--i18n-ru
  '((confirm-send-full . "magpt: Отправить staged‑дифф в LLM (%d байт)? ")
    (confirm-send-trunc . "magpt: Отправить staged‑дифф в LLM (исходно %d байт; отправим %d байт после усечения)? ")
    (request-llm-commit . "magpt: запрашиваем LLM для генерации сообщения коммита...")
    (result-copied . "magpt: результат показан в *magpt-commit* и скопирован в kill-ring")
    (empty-response . "magpt: пустой ответ от модели")
    (insertion-cancelled . "magpt: вставка отменена пользователем")
    (inserted-into-commit-buffer . "magpt: сообщение коммита вставлено в буфер коммита")
    (inserted-into-buffer-named . "magpt: сообщение коммита вставлено в %s")
    (gptel-error . "magpt: ошибка вызова gptel: %s")
    (gptel-error2 . "magpt/gptel ошибка: %s")
    (sending-cancelled . "magpt: отправка отменена пользователем")
    (no-staged-changes . "Нет застейдженных изменений (сделайте git add ...)")
    (replace-current-commit-msg? . "Заменить текущее сообщение коммита и вставить сгенерированное? ")
    (callback-error . "magpt: ошибка в callback: %s")
    ;; Обзор/История (ключи, используемые в AI Overview)
    (overview-response . "Ответ:")
    (json-opened . "Ответ открыт в JSON буфере")
    (json-copied . "Ответ скопирован в kill-ring")
    ;; Заголовки секций (локализованный UI)
    (overview-summary . "Сводка:")
    (overview-risks . "Риски:")
    (overview-suggestions . "Рекомендации:")
    (overview-lint-status . "Статус lint:")
    (overview-issues . "Проблемы:")
    (overview-suggestion . "Предложение:")
    (overview-name . "Имя:")
    (overview-alternatives . "Альтернативы:")
    (overview-rationale . "Обоснование:")
    (overview-steps . "Шаги:")
    ;; Заголовки карточек (локализованный UI)
    (overview-card-commit-lint . "Линт коммита / правка")
    (overview-card-branch-name . "Имя ветки (рекомендация)")
    (overview-card-resolve-conflict . "Разрешить конфликт (здесь)")
    (overview-card-push-pull . "Push/Pull — советы")
    (overview-card-branches . "Обзор веток")
    (overview-card-restore-file . "Восстановить файл (инструкция)")
    (overview-no-data . "(нет данных - нажмите [. g] для обновления)")
    (overview-stale . "(статус изменился - нажмите [. g] для обновления)")
    (patch-opened . "Патч открыт в буфере")))

(defconst magpt--i18n-fr
  '((confirm-send-full . "magpt: Envoyer le diff indexé au LLM (%d octets) ? ")
    (confirm-send-trunc . "magpt: Envoyer le diff indexé au LLM (original %d octets ; envoi de %d octets après troncature) ? ")
    (request-llm-commit . "magpt: demande au LLM de générer le message de commit...")
    (result-copied . "magpt: résultat copié dans le kill-ring et affiché dans *magpt-commit*")
    (empty-response . "magpt: réponse vide du modèle")
    (insertion-cancelled . "magpt: insertion annulée par l'utilisateur")
    (inserted-into-commit-buffer . "magpt: message de commit inséré dans le tampon de commit")
    (inserted-into-buffer-named . "magpt: message de commit inséré dans %s")
    (gptel-error . "magpt: erreur lors de l'appel à gptel : %s")
    (gptel-error2 . "erreur magpt/gptel : %s")
    (sending-cancelled . "magpt: envoi annulé par l'utilisateur")
    (no-staged-changes . "Aucun changement indexé trouvé (git add ...)")
    (replace-current-commit-msg? . "Remplacer le message de commit actuel et insérer celui généré ? ")
    (callback-error . "magpt: erreur dans le callback : %s")
    ;; Aperçu/Historiques (clés utilisées par l'aperçu IA)
    (overview-response . "Réponse :")
    (json-opened . "Réponse ouverte dans un tampon JSON")
    (json-copied . "Réponse copiée dans le kill-ring")
    ;; Titres de sections (UI localisée)
    (overview-summary . "Résumé :")
    (overview-risks . "Risques :")
    (overview-suggestions . "Suggestions :")
    (overview-lint-status . "Statut du lint :")
    (overview-issues . "Problèmes :")
    (overview-suggestion . "Suggestion :")
    (overview-name . "Nom :")
    (overview-alternatives . "Alternatives :")
    (overview-rationale . "Justification :")
    (overview-steps . "Étapes :")
    ;; Titres des cartes (UI localisée)
    (overview-card-commit-lint . "Commit Lint / Correction")
    (overview-card-branch-name . "Suggestion de nom de branche")
    (overview-card-resolve-conflict . "Résoudre le conflit (ici)")
    (overview-card-push-pull . "Conseils Push/Pull")
    (overview-card-branches . "Aperçu des branches")
    (overview-card-restore-file . "Restaurer un fichier (guide)")
    (overview-no-data . "(aucune donnée — appuyez sur [. g] pour actualiser)")
    (overview-stale . "(état modifié — appuyez sur [. g] pour actualiser)")
    (patch-opened . "Patch ouvert dans un tampon")))

(defun magpt--i18n (key &rest args)
  "Format localized message for KEY with ARGS using `magpt-info-language'."
  (magpt--maybe-load-rc)
  (let* ((lang (magpt--lang-code))
         (tbl (pcase lang
                ('ru magpt--i18n-ru)
                ('fr magpt--i18n-fr)
                (_   magpt--i18n-en)))
         (fmt (or (alist-get key tbl) (alist-get key magpt--i18n-en) "")))
    (magpt--log "i18n: key=%S lang=%S fmt=%s" key lang fmt)
    (if args (apply #'format fmt args) fmt)))


;;;; Section: Size control and prompt building
;;
;; We enforce UTF-8-safe truncation and build final prompts with clear markers.

(defun magpt--confirm-send (orig-bytes send-bytes)
  "Ask the user to confirm sending content of SEND-BYTES (showing ORIG-BYTES for context)."
  (if (not magpt-confirm-before-send)
      t
    (let ((msg (if (= orig-bytes send-bytes)
                   (magpt--i18n 'confirm-send-full send-bytes)
                 (magpt--i18n 'confirm-send-trunc orig-bytes send-bytes))))
      (magpt--log "confirm-send: info-lang=%S msg=%s" magpt-info-language msg)
      (y-or-n-p msg))))

;;;; Section: Task registry (experimental core abstraction)
;;
;; A Task encodes a flow: context → prompt → request → render/apply. This is the foundation
;; for the evolving assistant features beyond commit messages. It is optional and off by default.

(defcustom magpt-enable-task-registry t
  "If non-nil, expose experimental task registry commands (assist tasks; AI overview)."
  :type 'boolean
  :group 'magpt)

(cl-defstruct (magpt-task (:constructor magpt--task))
  "Task object holding necessary functions and metadata for execution."
  name title scope context-fn prompt-fn render-fn apply-fn confirm-send?)

(defvar magpt--tasks (make-hash-table :test 'eq)
  "Registry of magpt tasks keyed by symbol.")

(defun magpt--hash-table-keys (ht)
  "Return a list of keys in hash-table HT."
  (let (ks) (maphash (lambda (k _v) (push k ks)) ht) (nreverse ks)))

(defun magpt-register-task (task)
  "Register TASK (a `magpt-task' struct) in the registry."
  (puthash (magpt-task-name task) task magpt--tasks))

(defun magpt--run-task (task &optional ctx)
  "Run TASK: collect context, build prompt, request model, then render/apply."
  (magpt--log "run-task: START name=%s buffer=%s root=%s"
              (magpt-task-name task)
              (buffer-name)
              (ignore-errors (magpt--project-root)))
  (condition-case err
      (pcase-let* ((`(,data ,_preview ,bytes)
                    (progn
                      (magpt--log "run-task: %s collecting context..." (magpt-task-name task))
                      (funcall (magpt-task-context-fn task) ctx)))
                   (prompt
                    (progn
                      (magpt--log "run-task: %s building prompt (bytes=%s)..." (magpt-task-name task) (or bytes -1))
                      (funcall (magpt-task-prompt-fn task) data))))
        (if (and (or (null bytes) (zerop bytes)))
            (let ((name (magpt-task-name task)))
              (magpt--log "run-task: %s skipped (empty context)" name)
              (message "magpt: nothing to send for %s (empty context)" name))
          (let ((ok (if (magpt-task-confirm-send? task)
                        (magpt--confirm-send bytes bytes)
                      t)))
            (when ok
              (let ((gptel-model (or magpt-model gptel-model)))
                (magpt--log "run-task: %s bytes=%d info-lang=%S commit-lang=%S prompt-preview=%s"
                            (magpt-task-name task) (or bytes -1) magpt-info-language magpt-commit-language
                            (let ((n (min 180 (length prompt)))) (substring prompt 0 n)))
                (message "magpt: requesting %s..." (magpt-task-name task))
                (condition-case gerr
                    (let ((sys (pcase (magpt-task-name task)
                                 ((or 'stage-by-intent-hunks 'resolve-conflict-here) nil)
                                 (_ (magpt--system-prompt 'info)))))
                      (magpt--gptel-request
                       prompt
                       :system sys
                       :callback
                       (lambda (resp info)
                         (ignore info)
                         (let ((magpt--current-request prompt))
                           (condition-case cerr
                               (let ((out (string-trim (magpt--response->string resp))))
                                 (magpt--log "task-callback: %s resp-type=%S out-preview=%s"
                                             (magpt-task-name task) (type-of resp)
                                             (substring out 0 (min 180 (length out))))
                                 (funcall (magpt-task-render-fn task) out data)
                                 (when (magpt-task-apply-fn task)
                                   (funcall (magpt-task-apply-fn task) out data)))
                             (error
                              (magpt--log "task-callback exception: %s" (error-message-string cerr))
                              (magpt--log "task-callback exception: signal=%S data=%S" (car-safe cerr) (cdr-safe cerr))
                              (magpt--log "task-callback exception: BT:\n%s" (magpt--backtrace-string))
                              (message "%s" (magpt--i18n 'callback-error (error-message-string cerr)))))))))
                  (error
                   (magpt--log "gptel-request error for %s: %s\nBT:\n%s"
                               (magpt-task-name task)
                               (error-message-string gerr)
                               (magpt--backtrace-string))
                   (message "%s" (magpt--i18n 'gptel-error (error-message-string gerr)))))))))
        (error
         (magpt--log "run-task exception: %s" (error-message-string err))
         (magpt--log "run-task exception: signal=%S data=%S" (car-safe err) (cdr-safe err))
         (magpt--log "run-task exception: BT:\n%s" (magpt--backtrace-string))
         (message "%s" (magpt--i18n 'callback-error (error-message-string err)))))))

;;;###autoload
(defun magpt-run-task (name &optional ctx)
  "Interactively run a registered magpt task NAME. Experimental."
  (interactive
   (progn
     (unless magpt-enable-task-registry
       (user-error "Enable `magpt-enable-task-registry' to use experimental tasks"))
     (magpt--register-assist-tasks)
     (magpt--register-recommend-tasks)
     (list (intern (completing-read
                    "magpt task: "
                    (mapcar #'symbol-name (magpt--hash-table-keys magpt--tasks)))))))
  (magpt--maybe-load-rc)
  (when magpt-enable-task-registry
    (magpt--register-assist-tasks)
    (magpt--register-recommend-tasks))
  (magpt--log "run-task: pre-lookup name=%s tasks=%s"
              name (mapcar #'symbol-name (magpt--hash-table-keys magpt--tasks)))
  (let ((task (gethash name magpt--tasks)))
    (unless task
      (magpt--log "run-task: UNKNOWN task=%s (registry empty? %s)"
                  name (if (null (magpt--hash-table-keys magpt--tasks)) "t" "nil"))
      (user-error "Unknown magpt task: %s" name))
    (magpt--run-task task ctx)))

;;;; Section: History storage (read-only; used by AI Overview)
;;
;; A shared place to append prompts/responses and validity hints; visible in the Magit AI Overview.

(defvar magpt-history-changed-hook nil
  "Hook run after a history entry is appended.
UI modules (e.g., Magit overview) can subscribe to refresh themselves.")

(require 'magpt-history) ;; AI history storage (entries, append, search)

(defcustom magpt-ui-density 'regular
  "UI density profile (affects AI overview): 'regular or 'compact.
In compact mode, long lists are truncated (with hints) and spacing is reduced."
  :type '(choice (const :tag "Regular" regular)
                 (const :tag "Compact" compact))
  :group 'magpt)

(defcustom magpt-overview-compact-max-risks 3
  "Max number of risks to show in compact density for explain-status (AI overview)."
  :type 'integer
  :group 'magpt)

(defcustom magpt-overview-compact-max-suggestions 3
  "Max number of suggestions to show in compact density for explain-status (AI overview)."
  :type 'integer
  :group 'magpt)

(defface magpt-badge-info-face
  '((t :inherit shadow))
  "Neutral/info badge face (e.g., repo/branch chips) in overview header."
  :group 'magpt)

(provide 'magpt)

;;; magpt.el ends here
