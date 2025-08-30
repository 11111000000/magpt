;;; magpt.el --- MaGPT: Git/Magit AI assistant via gptel  -*- lexical-binding: t; -*-

;; Author: Peter <11111000000@email.com>
;; URL: https://github.com/11111000000/magpt
;; Version: 1.6.1
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

(declare-function magit-toplevel "ext:magit")
(declare-function magit-commit-create "ext:magit")
(declare-function transient-quit-all "ext:transient")
(defvar transient--prefix) ;; from transient

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
  "Load and apply user RC then project RC; project overrides user."
  ;; Load user-level first to establish defaults.
  (magpt--maybe-load-user-rc)
  ;; Then load project-level, which takes precedence.
  (magpt--maybe-load-project-rc))

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
    (json-cопied . "Ответ скопирован в kill-ring")
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

;;;; Section: GPT integration wrappers
;;
;; We centralize gptel-request usage for logging and callback safety. Streaming is opt-in.

(defun magpt--response->string (resp)
  "Return RESP as a string, tolerating backend variations."
  (cond
   ((stringp resp) resp)
   ((and (listp resp) (assq 'content resp))
    (let ((c (cdr (assq 'content resp))))
      (if (stringp c) c (format "%S" resp))))
   ((hash-table-p resp)
    (condition-case _ (json-encode resp) (error (format "%S" resp))))
   ((listp resp)
    (condition-case _ (json-encode resp) (error (format "%S" resp))))
   (t (format "%S" resp))))

(defun magpt--sanitize-response (s)
  "Sanitize LLM response S: strip common Markdown fences and leading labels."
  (let* ((s (string-trim (or s ""))))
    ;; Strip leading 'Answer:'/'Ответ:' labels (common model prefixes)
    (setq s (replace-regexp-in-string "\\`\\(?:\\s-*\\(Answer\\|Ответ\\)[:：].*\\n+\\)+" "" s))
    ;; Strip outer Markdown fences (=lang ... = or ~~~)
    (let* ((lines (split-string s "\n"))
           (first (car lines))
           (last  (car (last lines))))
      (when (and first last
                 (string-match-p "\\`[`~]\\{3,\\}" first)
                 (string-match-p "\\`[`~]\\{3,\\}[ \t]*\\'" last))
        (setq s (mapconcat #'identity (butlast (cdr lines)) "\n"))))
    s))

(defun magpt--safe-callback (cb)
  "Wrap CB to prevent hard failures; log diagnostics on error."
  (lambda (resp info)
    (when magpt-log-enabled
      (let* ((ty (type-of resp))
             (preview (condition-case _
                          (let* ((s (magpt--response->string resp))
                                 (n (min 180 (length s))))
                            (substring s 0 n))
                        (error "<unprintable>"))))
        (magpt--log "safe-callback: resp-type=%S preview=%s info=%S"
                    ty preview (and (listp info)
                                    (ignore-errors
                                      (cl-subseq info 0 (min 10 (length info))))))))
    (condition-case err
        (funcall cb resp info)
      (error
       (magpt--log "callback exception: %s" (error-message-string err))
       (magpt--log "callback exception: signal=%S data=%S" (car-safe err) (cdr-safe err))
       (magpt--log "callback exception: BT:\n%s" (magpt--backtrace-string))
       (message "%s" (magpt--i18n 'callback-error (error-message-string err)))))))

(defun magpt--gptel-request (prompt &rest args)
  "Call `gptel-request' with PROMPT and ARGS, adding logging and safe callback."
  (let* ((plist args)
         (cb (plist-get plist :callback))
         (stream (plist-get plist :stream)))
    (when magpt-log-enabled
      (magpt--log "gptel-request: model=%S stream=%S prompt-len=%d preview=%s"
                  (or magpt-model gptel-model)
                  stream
                  (length (or prompt ""))
                  (let* ((p (or prompt "")) (n (min 180 (length p))))
                    (substring p 0 n))))
    (when cb
      (setq plist (plist-put plist :callback (magpt--safe-callback cb))))
    (apply #'gptel-request prompt plist)))

(defun magpt--system-prompt (kind)
  "Return a strict system directive based on language and KIND ('commit or 'info)."
  (let* ((lang (pcase kind
                 ('commit magpt-commit-language)
                 (_      magpt-info-language)))
         (l (and (stringp lang) (> (length lang) 0) lang)))
    (when l
      (format "Answer STRICTLY in %s. Do not use any other language." l))))

;;;; Section: Git plumbing helpers
;;
;; These helpers encapsulate git invocation and project root detection.

(defun magpt--executable-git ()
  "Return the path to the git executable or nil."
  (executable-find "git"))

;; Per-repo recent git output (stdout/stderr), for Explain Status suggestions.
(defcustom magpt-recent-git-output-lines 80
  "How many last lines of Git output to keep per repository."
  :type 'integer
  :group 'magpt)

(defvar magpt--recent-git-output-map (make-hash-table :test 'equal)
  "Map ROOT-KEY → last lines of git output (newline-separated string).")

(defun magpt--recent-git--key (dir)
  "Return the map key for recent output by directory DIR."
  (file-name-as-directory (expand-file-name (or dir default-directory))))

(defun magpt--recent-git-output-get (dir)
  "Return saved recent output for directory DIR (as a string)."
  (or (gethash (magpt--recent-git--key dir) magpt--recent-git-output-map) ""))

(defun magpt--recent-git-output-append (dir exit args out)
  "Append an entry for DIR to the log with EXIT, command ARGS, and output OUT."
  (let* ((key (magpt--recent-git--key dir))
         (hdr (format "%d git %s" exit (mapconcat #'identity args " ")))
         (lines (append (list hdr)
                        (and (stringp out) (split-string out "\n" t))))
         (prev (and (gethash key magpt--recent-git-output-map)
                    (split-string (gethash key magpt--recent-git-output-map) "\n" t)))
         (all (append (or prev '()) lines))
         (keep (last all (max 1 magpt-recent-git-output-lines))))
    (puthash key (string-join keep "\n") magpt--recent-git-output-map)))

(defun magpt--process-git (dir &rest args)
  "Execute git with ARGS in DIR. Return (EXIT-CODE . STRING-OUTPUT)."
  (let ((default-directory (file-name-as-directory (or dir default-directory))))
    (with-temp-buffer
      (let* ((start (float-time)))
        (magpt--log "process-git: start dir=%s args=%s"
                    default-directory (mapconcat #'identity args " "))
        (let* ((exit (apply #'process-file (or (magpt--executable-git) "git")
                            nil t nil args))
               (out  (buffer-string))
               (trimmed (if (string-suffix-p "\n" out) (substring out 0 -1) out))
               (dur (- (float-time) start)))
          (magpt--log "process-git: end exit=%s dur=%.3fs bytes=%d"
                      exit dur (length trimmed))
          ;; Record into per-repo log (for Explain Status → RECENT GIT OUTPUT)
          (ignore-errors (magpt--recent-git-output-append default-directory exit args trimmed))
          (cons exit trimmed))))))

(defun magpt--git (dir &rest args)
  "Execute git ARGS in DIR and return the output string or signal user-error."
  (pcase (apply #'magpt--process-git dir args)
    (`(,exit . ,out)
     (if (zerop exit)
         out
       (user-error "Git error (%s): %s" exit out)))))

(defun magpt--git-apply-temp (dir patch &rest args)
  "Apply PATCH (string) via 'git apply' in DIR with ARGS, using a temp file.
Signal a user-error on non-zero exit."
  (let ((tmp (make-temp-file "magpt-patch" nil ".patch")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (string-trim-right (or patch "")) "\n"))
          (apply #'magpt--git dir "apply" (append args (list tmp))))
      (ignore-errors (delete-file tmp)))))

(defun magpt--git-apply-check-temp (dir patch &rest args)
  "Run 'git apply --check' for PATCH (string) in DIR with ARGS.
Return (EXIT-CODE . OUTPUT) without signaling."
  (let ((tmp (make-temp-file "magpt-patch" nil ".patch")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (string-trim-right (or patch "")) "\n"))
          (apply #'magpt--process-git dir "apply" (append '("--check") args (list tmp))))
      (ignore-errors (delete-file tmp)))))

(defun magpt--git-root-from (dir)
  "Return the Git repo root directory for DIR, or nil if not found."
  (when (and (magpt--executable-git) (file-directory-p dir))
    (let ((res (magpt--process-git dir "rev-parse" "--show-toplevel")))
      (when (eq (car res) 0)
        (file-name-as-directory (cdr res))))))

(defun magpt--try-root-from-magit ()
  "Return repo root using Magit, or nil if unavailable."
  (when (and (featurep 'magit) (fboundp 'magit-toplevel))
    (ignore-errors
      (let ((root (magit-toplevel)))
        (when (and (stringp root) (file-directory-p root))
          (file-name-as-directory root))))))

(defun magpt--try-root-from-vc ()
  "Return repo root using VC, or nil if unavailable."
  (let ((root (ignore-errors (vc-root-dir))))
    (when (and (stringp root) (file-directory-p root))
      (file-name-as-directory root))))

(defun magpt--try-root-from-project ()
  "Return project root using project.el, or nil if unavailable."
  (let* ((proj (ignore-errors (project-current)))
         (root (when proj (ignore-errors (project-root proj)))))
    (when (and (stringp root) (file-directory-p root))
      (file-name-as-directory root))))

(defun magpt--project-root ()
  "Determine the Git project root according to `magpt-project-root-strategy'.
Signal an error if no repository is found."
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
         (root-s (or (seq-some (lambda (f) (funcall f)) candidates)
                     (and (file-directory-p default-directory)
                          (magpt--git-root-from default-directory)))))
    (unless root-s
      (user-error "No Git repository found for current directory"))
    (file-name-as-directory (expand-file-name root-s))))

(defun magpt--staged-diff (root)
  "Return the diff string for staged changes in ROOT."
  (apply #'magpt--git root "diff" (or magpt-diff-args '("--staged" "--no-color"))))

(defun magpt--string-bytes (s)
  "Return UTF-8 byte size of string S."
  (if (stringp s) (string-bytes s) 0))

;;;; Section: Size control and prompt building
;;
;; We enforce UTF-8-safe truncation and build final prompts with clear markers.

(defun magpt--truncate-to-bytes (s max-bytes)
  "Return the longest prefix of S whose UTF-8 size ≤ MAX-BYTES.
Uses binary search on character count to align with UTF-8 boundaries."
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
                (setq best mid lo mid)
              (setq hi (1- mid)))))
        (substring s 0 best)))))

(defun magpt--maybe-truncate (s max-bytes)
  "Return cons (TEXT . TRUNCATEDP) where TEXT is S or a truncated prefix."
  (if (or (null max-bytes) (<= (string-bytes s) max-bytes))
      (cons s nil)
    (let ((tstr (magpt--truncate-to-bytes s max-bytes)))
      (cons tstr t))))

(defun magpt--build-commit-prompt (template diff &optional truncatedp)
  "Build the final prompt for commit message generation from TEMPLATE and DIFF.
Appends a language directive when `magpt-commit-language' is non-nil."
  (let* ((tpl (string-trim-right (or template "")))
         (lang (and (stringp magpt-commit-language)
                    (> (length magpt-commit-language) 0)
                    magpt-commit-language)))
    (concat
     tpl
     (if lang
         (format "\n\nAnswer STRICTLY in %s. Write the commit message only in this language." lang)
       "")
     "\n\n--- BEGIN DIFF ---\n"
     diff
     (when truncatedp "\n\n[diff truncated due to size limit]")
     "\n--- END DIFF ---\n")))

(defun magpt--confirm-send (orig-bytes send-bytes)
  "Ask the user to confirm sending content of SEND-BYTES (showing ORIG-BYTES for context)."
  (if (not magpt-confirm-before-send)
      t
    (let ((msg (if (= orig-bytes send-bytes)
                   (magpt--i18n 'confirm-send-full send-bytes)
                 (magpt--i18n 'confirm-send-trunc orig-bytes send-bytes))))
      (magpt--log "confirm-send: info-lang=%S msg=%s" magpt-info-language msg)
      (y-or-n-p msg))))

;;;; Section: Commit buffer detection and insertion
;;
;; We preserve commit buffer structure and only replace the message part, not comments.

(defun magpt--commit-buffer-p (&optional buf)
  "Return non-nil if BUF appears to be a commit message buffer."
  (with-current-buffer (or buf (current-buffer))
    (or (derived-mode-p 'git-commit-mode)
        (and buffer-file-name
             (string-equal (file-name-nondirectory buffer-file-name)
                           "COMMIT_EDITMSG"))
        (bound-and-true-p with-editor-mode))))

(defun magpt--find-commit-buffer ()
  "Find a live commit message buffer if any."
  (catch 'found
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (magpt--commit-buffer-p buf))
        (throw 'found buf)))
    nil))

(defun magpt--commit-comment-char ()
  "Return the comment character used by git-commit, or fallback '#'."
  (or (and (boundp 'git-commit-comment-char) git-commit-comment-char)
      (and (boundp 'comment-start)
           (stringp comment-start)
           (> (length comment-start) 0)
           (aref comment-start 0))
      ?#))

(defun magpt--commit-message-boundaries ()
  "Return (MSG-END . COMMENTS-BEG) for current commit buffer.
If a trailing comments block exists (preceded by a blank line), MSG-END is before it.
Otherwise MSG-END is point-max and COMMENTS-BEG is nil."
  (save-excursion
    (goto-char (point-min))
    (let* ((c (magpt--commit-comment-char))
           (rx (format "^%c" c))
           (cand nil)
           pos)
      (while (and (not cand)
                  (setq pos (re-search-forward rx nil t)))
        (let ((bol (match-beginning 0)))
          (when (save-excursion
                  (goto-char bol)
                  (forward-line -1)
                  (or (bobp) (looking-at-p "^[ \t]*$")))
            (let ((ok t))
              (save-excursion
                (goto-char bol)
                (while (and ok (not (eobp)))
                  (cond
                   ((looking-at-p rx))
                   ((looking-at-p "^[ \t]*$"))
                   (t (setq ok nil)))
                  (forward-line 1)))
              (when ok (setq cand bol))))))
      (if cand
          (cons cand cand)
        (cons (point-max) nil)))))

(defun magpt--insert-into-commit-buffer-target (buf text)
  "Insert TEXT into commit buffer BUF, preserving comment block at the bottom.
Asks for confirmation if an existing message is present."
  (when (and (buffer-live-p buf))
    (with-current-buffer buf
      (when (magpt--commit-buffer-p)
        (let* ((bounds (magpt--commit-message-boundaries))
               (msg-end (car bounds))
               (existing (string-trim (buffer-substring-no-properties (point-min) msg-end))))
          (when (or (string-empty-p existing)
                    (y-or-n-p (magpt--i18n 'replace-current-commit-msg?)))
            (let ((inhibit-read-only t))
              (delete-region (point-min) msg-end)
              (goto-char (point-min))
              (insert (string-trim-right text) "\n")
              (goto-char (point-min)))
            (message "%s" (format (magpt--i18n 'inserted-into-buffer-named) (buffer-name buf)))
            t))))))

(defun magpt--insert-into-commit-buffer (text)
  "Insert TEXT into the active commit buffer if available.
Return non-nil if insertion happened."
  (let ((target (or (and (magpt--commit-buffer-p) (current-buffer))
                    (magpt--find-commit-buffer))))
    (when target
      (magpt--insert-into-commit-buffer-target target text))))

(defun magpt--show-in-output-buffer (text)
  "Show TEXT in a read-only buffer and copy to kill-ring."
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
    (message "%s" (magpt--i18n 'result-copied))))

;;;; Section: Progress overlays and spinner (commit buffer)
;;
;; Non-blocking UI feedback while awaiting model responses. Cleans up on all paths.

(defcustom magpt-commit-overlay-text "Message generation..."
  "Overlay text shown in the commit buffer while generation is in progress."
  :type 'string
  :group 'magpt)

(defface magpt-commit-overlay-face
  '((t :inherit font-lock-comment-delimiter-face :weight bold))
  "Face for the commit message generation overlay."
  :group 'magpt)

(defvar-local magpt--commit-overlay nil
  "Overlay shown in the commit buffer while message generation is in progress.")

(defun magpt--show-commit-overlay (buf)
  "Show an overlay in BUF to indicate commit message generation is in progress."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (unless (overlayp magpt--commit-overlay)
          (setq magpt--commit-overlay (make-overlay (point-min) (point-min) buf t t)))
        (overlay-put magpt--commit-overlay 'before-string
                     (propertize (concat magpt-commit-overlay-text "\n")
                                 'face 'magpt-commit-overlay-face))))))

(defun magpt--remove-commit-overlay (buf)
  "Remove overlay in BUF if present."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (overlayp magpt--commit-overlay)
        (delete-overlay magpt--commit-overlay)
        (setq magpt--commit-overlay nil)))))

;;;; Section: Commit message generation (core public commands)
;;
;; Two entry points:
;; - magpt-generate-commit-message: use inside an existing commit buffer (or show in a buffer).
;; - magpt-commit-staged          : open a Magit commit buffer and auto-fill.

;;;###autoload
(defun magpt-generate-commit-message ()
  "Generate a commit message from the staged diff using gptel.
If a commit buffer exists and `magpt-insert-into-commit-buffer' is non-nil,
insert the result there; otherwise show it in *magpt-commit*."
  (interactive)
  (magpt--maybe-load-rc)
  (unless (magpt--executable-git)
    (user-error "Could not find executable 'git' in PATH"))
  (let* ((target (when magpt-insert-into-commit-buffer
                   (or (and (magpt--commit-buffer-p) (current-buffer))
                       (magpt--find-commit-buffer))))
         (root (magpt--project-root))
         (diff (magpt--staged-diff root)))
    (when (string-empty-p (string-trim diff))
      (user-error "%s" (magpt--i18n 'no-staged-changes)))
    (let* ((orig-bytes (string-bytes diff))
           (trunc (magpt--maybe-truncate diff magpt-max-diff-bytes))
           (diff (car trunc))
           (truncatedp (cdr trunc))
           (send-bytes (string-bytes diff))
           (prompt (magpt--build-commit-prompt magpt-commit-prompt diff truncatedp)))
      (magpt--log "gen-commit: orig=%d send=%d truncated=%s model=%S commit-lang=%S info-lang=%S prompt-preview=%s"
                  orig-bytes send-bytes truncatedp
                  (or magpt-model gptel-model) magpt-commit-language magpt-info-language
                  (let ((n (min 180 (length prompt))))
                    (substring prompt 0 n)))
      (if (magpt--confirm-send orig-bytes send-bytes)
          (progn
            (when target (magpt--show-commit-overlay target))
            (message "%s" (magpt--i18n 'request-llm-commit))
            (condition-case err
                (let ((gptel-model (or magpt-model gptel-model)))
                  (magpt--gptel-request
                   prompt
                   :system (magpt--system-prompt 'commit)
                   :context target
                   :callback #'magpt--commit-callback))
              (error
               (when target (magpt--remove-commit-overlay target))
               (message "%s" (magpt--i18n 'gptel-error (error-message-string err))))))
        (message "%s" (magpt--i18n 'sending-cancelled))))))

(defun magpt--commit-callback (response info)
  "Callback for gptel commit generation. Insert or display the result."
  (condition-case err
      (let* ((errstr (plist-get info :error))
             (commit-buf (plist-get info :context))
             (target (or (and (buffer-live-p commit-buf)
                              (magpt--commit-buffer-p commit-buf)
                              commit-buf)
                         (magpt--find-commit-buffer)))
             (text (string-trim (magpt--response->string response))))
        (magpt--log "commit-callback: type=%S err=%S text-preview=%s"
                    (type-of response) errstr
                    (let ((s (magpt--response->string response)))
                      (substring s 0 (min 180 (length s)))))
        (cond
         (errstr
          (when (buffer-live-p commit-buf)
            (magpt--remove-commit-overlay commit-buf))
          (message "%s" (magpt--i18n 'gptel-error2 errstr)))
         ((string-empty-p text)
          (when (buffer-live-p commit-buf)
            (magpt--remove-commit-overlay commit-buf))
          (message "%s" (magpt--i18n 'empty-response)))
         ((and magpt-insert-into-commit-buffer target)
          (magpt--remove-commit-overlay target)
          (if (magpt--insert-into-commit-buffer-target target text)
              (message "%s" (magpt--i18n 'inserted-into-commit-buffer))
            (message "%s" (magpt--i18n 'insertion-cancelled))))
         (t
          (when (buffer-live-p commit-buf)
            (magpt--remove-commit-overlay commit-buf))
          (magpt--show-in-output-buffer text))))
    (error
     (message "%s" (magpt--i18n 'callback-error (error-message-string err))))))

;;;###autoload
(defun magpt-commit-staged ()
  "Open a Magit commit buffer (if needed) and insert a generated message for staged changes.
Does not perform the commit; use standard C-c C-c to finalize. Requires Magit."
  (interactive)
  (when (and (featurep 'transient)
             (boundp 'transient--prefix)
             (bound-and-true-p transient--prefix)
             (fboundp 'transient-quit-all))
    (transient-quit-all))
  (run-at-time 0 nil #'magpt--commit-staged-run))

(defun magpt--commit-staged-run ()
  "Implementation for `magpt-commit-staged'."
  (unless (magpt--executable-git)
    (user-error "Could not find executable 'git' in PATH"))
  (magpt--maybe-load-rc)
  (unless (and (require 'magit nil t) (fboundp 'magit-commit-create))
    (user-error "magpt-commit-staged requires Magit"))
  (let* ((root (magpt--project-root))
         (diff (magpt--staged-diff root)))
    (if (string-empty-p (string-trim diff))
        (message "%s" (magpt--i18n 'no-staged-changes))
      (let* ((orig-bytes (string-bytes diff))
             (trunc (magpt--maybe-truncate diff magpt-max-diff-bytes))
             (diff (car trunc))
             (truncatedp (cdr trunc))
             (send-bytes (string-bytes diff))
             (prompt (magpt--build-commit-prompt magpt-commit-prompt diff truncatedp))
             (target-buf (and (magpt--commit-buffer-p) (current-buffer))))
        (if (and target-buf (buffer-live-p target-buf))
            (if (magpt--confirm-send orig-bytes send-bytes)
                (progn
                  (magpt--show-commit-overlay target-buf)
                  (message "%s" (magpt--i18n 'request-llm-commit))
                  (condition-case err
                      (let ((gptel-model (or magpt-model gptel-model)))
                        (magpt--gptel-request
                         prompt
                         :system (magpt--system-prompt 'commit)
                         :context target-buf
                         :callback #'magpt--commit-callback))
                    (error
                     (magpt--remove-commit-overlay target-buf)
                     (message "%s" (magpt--i18n 'gptel-error (error-message-string err))))))
              (message "%s" (magpt--i18n 'sending-cancelled)))
          (if (magpt--confirm-send orig-bytes send-bytes)
              (let (hook-fn remove-timer)
                (setq hook-fn
                      (lambda ()
                        (when (timerp remove-timer) (cancel-timer remove-timer))
                        (remove-hook 'git-commit-setup-hook hook-fn)
                        (let ((buf (current-buffer)))
                          (when (magpt--commit-buffer-p buf)
                            (magpt--show-commit-overlay buf)
                            (message "%s" (magpt--i18n 'request-llm-commit))
                            (condition-case err
                                (let ((gptel-model (or magpt-model gptel-model)))
                                  (magpt--gptel-request
                                   prompt
                                   :system (magpt--system-prompt 'commit)
                                   :context buf
                                   :callback #'magpt--commit-callback))
                              (error
                               (magpt--remove-commit-overlay buf)
                               (message "%s" (magpt--i18n 'gptel-error (error-message-string err)))))))))
                (add-hook 'git-commit-setup-hook hook-fn)
                (setq remove-timer
                      (run-at-time 20 nil
                                   (lambda ()
                                     (remove-hook 'git-commit-setup-hook hook-fn))))
                (condition-case err
                    (magit-commit-create '())
                  (error
                   (when (timerp remove-timer) (cancel-timer remove-timer))
                   (remove-hook 'git-commit-setup-hook hook-fn)
                   (message "magpt: could not open Magit commit buffer: %s" (error-message-string err)))))
            (message "%s" (magpt--i18n 'sending-cancelled))))))))

;;;; Section: Magit integration minor mode (transient entry)
;;
;; Adds a button “[i] Commit with AI message (magpt)” to Magit’s commit transient.

(defface magpt-transient-face
  '((t :inherit font-lock-keyword-face :foreground "green3" :weight bold))
  "Face for MaGPT entries in Magit transient menus."
  :group 'magpt)



(defun magpt--transient-desc (s)
  "Return S; kept for future styling hooks."
  s)

;;;###autoload

;; Safe helpers to integrate with Transient across versions (avoid hard failures).
(defun magpt--transient-append-suffix-safe (parent pos spec)
  "Try to append SPEC after POS in PARENT transient. Return non-nil on success."
  (when (featurep 'transient)
    (condition-case err
        (prog1 t (transient-append-suffix parent pos spec))
      (error
       (magpt--log "transient append failed: parent=%S pos=%S err=%s"
                   parent pos (error-message-string err))
       nil))))

(defun magpt--transient-remove-suffix-safe (parent key)
  "Try to remove KEY from PARENT transient without throwing."
  (when (featurep 'transient)
    (ignore-errors (transient-remove-suffix parent key))))

(defun magpt--transient-add-to-magit-dispatch ()
  "Best-effort add magpt entries to `magit-dispatch' across Magit/Transient versions."
  (when (featurep 'transient)
    (let ((anchors '("!" "V" "B" "h" "t")))
      (cl-labels ((try (spec)
                    (or (seq-some (lambda (a)
                                    (magpt--transient-append-suffix-safe 'magit-dispatch a spec))
                                  anchors)
                        (magpt--transient-append-suffix-safe 'magit-dispatch nil spec))))
        (try `("." ,(magpt--transient-desc "AI actions (magpt)") magpt-ai-actions))))))

;;;###autoload
(define-minor-mode magpt-mode
  "Global minor mode: integrate MaGPT with Magit’s commit transient."
  :global t
  :group 'magpt
  (if magpt-mode
      (with-eval-after-load 'magit
        ;; Commit transient: add AI commit entry
        (transient-append-suffix 'magit-commit "c"
          `("i" ,(magpt--transient-desc "Commit with AI message (magpt)") magpt-commit-staged))
        ;; Magit dispatch: robust insertion (no hard dependency on a specific anchor).
        (magpt--transient-add-to-magit-dispatch)
        ;; Direct key in Magit Status buffer: "." opens AI actions immediately (without dispatch).
        (when (boundp 'magit-status-mode-map)
          (define-key magit-status-mode-map (kbd ".") #'magpt-ai-actions-entry))
        ;; Magit Status: AI overview section (read-only; no background calls)
        ;; Append to run after built-in sections (e.g., after Recent commits).
        (add-hook 'magit-status-sections-hook #'magpt-magit-insert-ai-overview t))
    (with-eval-after-load 'magit
      (magpt--transient-remove-suffix-safe 'magit-commit "i")
      (magpt--transient-remove-suffix-safe 'magit-dispatch ".")
      ;; Unbind our direct key when disabling mode.
      (when (boundp 'magit-status-mode-map)
        (define-key magit-status-mode-map (kbd ".") nil))
      (remove-hook 'magit-status-sections-hook #'magpt-magit-insert-ai-overview))))

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
                    (magpt--gptel-request
                     prompt
                     :system (magpt--system-prompt 'info)
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
                            (message "%s" (magpt--i18n 'callback-error (error-message-string cerr))))))))
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





(defvar magpt--history-entries nil
  "List of history entries (plists):
  :time STRING :task SYMBOL :request STRING :response STRING
  :valid t/nil :note STRING (optional).")

(defvar magpt--current-request nil
  "Dynamically bound prompt/request preview for history appends and AI overview rendering.")

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

(defun magpt--entry-parse-json-safe (entry)
  "Parse ENTRY's :response as JSON; return alist or nil."
  (let ((resp (plist-get entry :response)))
    (condition-case _err
        (json-parse-string (or resp "") :object-type 'alist :array-type 'list)
      (error nil))))

(defun magpt--btn-preview-text (title text &optional mode)
  "Open TEXT in a preview buffer with TITLE; MODE selects major-mode symbol."
  (let ((buf (get-buffer-create "*magpt-preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-trim-right text) "\n")
        (goto-char (point-min))
        (cond
         ((eq mode 'json) (when (fboundp 'json-mode) (ignore-errors (json-mode)))
          (condition-case _ (when (fboundp 'json-pretty-print-buffer)
                              (json-pretty-print-buffer))
            (error nil)))
         ((eq mode 'shell) (ignore-errors (sh-mode)))
         (t (text-mode)))
        (setq buffer-read-only t)))
    (pop-to-buffer buf)
    (rename-buffer (format "*magpt-preview: %s*" title) t)))

;; Commit Lint/Fix — helpers

(defun magpt--extract-commit-lint-message (entry)
  "Return suggestion.message from commit-lint-suggest ENTRY, or nil."
  (let* ((data (magpt--entry-parse-json-safe entry))
         (sug  (alist-get 'suggestion data))
         (msg  (and (listp sug) (alist-get 'message sug))))
    (and (stringp msg) msg)))

(defun magpt--btn-copy-commit-message (button)
  "Copy suggested commit message from commit-lint-suggest entry."
  (let* ((e (button-get button 'magpt-entry))
         (msg (magpt--extract-commit-lint-message e)))
    (if (stringp msg)
        (progn (kill-new msg) (message "%s" (magpt--i18n 'json-copied)))
      (user-error "No suggested message in this entry"))))

(defun magpt--btn-preview-commit-message (button)
  "Preview suggested commit message in a read-only buffer."
  (let* ((e (button-get button 'magpt-entry))
         (msg (magpt--extract-commit-lint-message e)))
    (if (stringp msg)
        (magpt--btn-preview-text "Commit Lint suggestion" msg 'text)
      (user-error "No suggested message in this entry"))))

(defun magpt--btn-insert-commit-message (button)
  "Insert suggested commit message into a live commit buffer (with confirmation)."
  (let* ((e (button-get button 'magpt-entry))
         (msg (magpt--extract-commit-lint-message e))
         (target (or (and (magpt--commit-buffer-p) (current-buffer))
                     (magpt--find-commit-buffer))))
    (unless (stringp msg) (user-error "No suggested message in this entry"))
    (unless target (user-error "No commit buffer found"))
    (if (magpt--insert-into-commit-buffer-target target msg)
        (message "%s" (magpt--i18n 'inserted-into-commit-buffer))
      (message "%s" (magpt--i18n 'insertion-cancelled)))))

;; Branch Name Suggest — helpers

(defun magpt--extract-branch-name (entry)
  "Return branch name string from branch-name-suggest ENTRY, or nil."
  (let* ((data (magpt--entry-parse-json-safe entry))
         (name (alist-get 'name data)))
    (and (stringp name) (> (length name) 0) name)))

(defun magpt--extract-branch-rationale (entry)
  "Return rationale string from branch-name-suggest ENTRY, or nil."
  (let* ((data (magpt--entry-parse-json-safe entry))
         (rat (alist-get 'rationale data)))
    (and (stringp rat) rat)))

(defun magpt--btn-copy-branch-name (button)
  "Copy suggested branch name."
  (let* ((e (button-get button 'magpt-entry))
         (name (magpt--extract-branch-name e)))
    (if (stringp name)
        (progn (kill-new name) (message "%s" (magpt--i18n 'json-copied)))
      (user-error "No branch name available"))))

(defun magpt--btn-copy-branch-rationale (button)
  "Copy branch rationale text."
  (let* ((e (button-get button 'magpt-entry))
         (rat (magpt--extract-branch-rationale e)))
    (if (stringp rat)
        (progn (kill-new rat) (message "%s" (magpt--i18n 'json-copied)))
      (user-error "No rationale available"))))

(defun magpt--btn-create-branch (button)
  "Create branch from suggestion (git switch -c NAME), gated by magpt-allow-apply-safe-ops."
  (unless magpt-allow-apply-safe-ops
    (user-error "Applying operations is disabled (magpt-allow-apply-safe-ops is nil)"))
  (let* ((e (button-get button 'magpt-entry))
         (name (magpt--extract-branch-name e)))
    (unless (stringp name) (user-error "No branch name available"))
    (when (y-or-n-p (format "Create and switch to branch '%s'? " name))
      (condition-case err
          (let ((root (magpt--project-root)))
            (magpt--git root "switch" "-c" name)
            (message "magpt: created and switched to %s" name))
        (error (user-error "Git error: %s" (error-message-string err)))))))

(defun magpt--entry-at-point ()
  "Return the history ENTRY plist at point, or nil if none.
Relies on the 'magpt-entry text property provided by UI buttons/sections."
  (let ((pos (point)) entry)
    (setq entry (get-text-property pos 'magpt-entry))
    (while (and (null entry) (> pos (point-min)))
      (setq pos (1- (or (previous-single-property-change pos 'magpt-entry nil (point-min))
                        (point-min))))
      (setq entry (get-text-property pos 'magpt-entry)))
    entry))

(defun magpt-open-response-json (&optional entry)
  "Open ENTRY's response in a JSON buffer and pretty-print when valid.
When called interactively without ENTRY, try entry at point or the latest."
  (interactive)
  (let* ((e (or entry (magpt--entry-at-point) (car (last magpt--history-entries)))))
    (unless e (user-error "No history entry available"))
    (let* ((resp (plist-get e :response))
           (buf (get-buffer-create "*magpt-json*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (string-trim-right resp) "\n")
          (goto-char (point-min))
          (when (fboundp 'json-mode) (ignore-errors (json-mode)))
          (condition-case _err
              (when (fboundp 'json-pretty-print-buffer)
                (json-pretty-print-buffer))
            (error nil))
          (setq buffer-read-only t)))
      (pop-to-buffer buf)
      (message "%s" (magpt--i18n 'patch-opened)))))

(defun magpt--btn--call (fn entry)
  "Helper to call FN with ENTRY, catching errors."
  (condition-case err
      (funcall fn entry)
    (error (message "magpt: %s" (error-message-string err)))))

(defun magpt--btn-copy (button)
  (let ((e (button-get button 'magpt-entry)))
    (magpt--btn--call
     (lambda (entry)
       (let ((resp (plist-get entry :response)))
         (kill-new resp)
         (message "%s" (magpt--i18n 'json-copied))))
     e)))

(defun magpt--btn-open-json (button)
  (let ((e (button-get button 'magpt-entry)))
    (magpt--btn--call
     (lambda (entry) (magpt-open-response-json entry))
     e)))

(defun magpt--apply-stage-by-intent-entry (entry)
  "Apply stage/unstage plan from ENTRY (stage-by-intent)."
  (let* ((data (magpt--entry-ensure-json entry))
         (groups (or (alist-get 'groups data) '()))
         (ops (cl-loop for g in groups append
                       (let ((files (alist-get 'files g)))
                         (cl-loop for f in files
                                  for action = (alist-get 'action f)
                                  for path = (alist-get 'path f)
                                  when (and (member action '("stage" "unstage"))
                                            (stringp path) (> (length path) 0))
                                  collect (cons action path))))))
    (when (null ops) (user-error "No file operations to apply"))
    (let* ((root (magpt--project-root))
           (preview (mapconcat
                     (lambda (op)
                       (pcase op
                         (`("stage" . ,p)   (format "git add -- %s" p))
                         (`("unstage" . ,p) (format "git restore --staged -- %s" p))
                         (_ (format "# skip %S" op))))
                     ops "\n")))
      (when (y-or-n-p (format "Apply file staging plan?\n%s\nProceed? " preview))
        (dolist (op ops)
          (pcase op
            (`("stage" . ,p)   (ignore-errors (magpt--git root "add" "--" p)))
            (`("unstage" . ,p) (ignore-errors (magpt--git root "restore" "--staged" "--" p)))
            (_ nil)))
        (message "magpt: applied staging plan; review in Magit status")))))

(defun magpt--btn-apply-stage-plan (button)
  (unless magpt-allow-apply-safe-ops
    (user-error "Applying operations is disabled (magpt-allow-apply-safe-ops is nil)"))
  (let ((e (button-get button 'magpt-entry)))
    (magpt--btn--call #'magpt--apply-stage-by-intent-entry e)))

(defun magpt-open-response-patch (&optional entry)
  "Open ENTRY's response in a diff-mode buffer."
  (interactive)
  (let* ((e (or entry (magpt--entry-at-point)))
         (resp (plist-get e :response))
         (buf (get-buffer-create "*magpt-patch*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-trim-right resp) "\n")
        (goto-char (point-min))
        (when (fboundp 'diff-mode) (ignore-errors (diff-mode)))
        (setq buffer-read-only t)))
    (pop-to-buffer buf)
    (message "%s" (magpt--i18n 'json-opened))))

(defun magpt-check-response-patch (&optional entry &rest args)
  "Run 'git apply --check' for ENTRY's response patch in index or worktree (per ARGS)."
  (interactive)
  (let* ((e (or entry (magpt--entry-at-point)))
         (resp (plist-get e :response))
         (root (magpt--project-root))
         (res (apply #'magpt--git-apply-check-temp root resp args))
         (exit (car res))
         (out  (cdr res)))
    (if (zerop exit)
        (message "magpt: patch --check OK")
      (message "magpt: patch --check failed: %s" out))))

(defun magpt--btn-open-patch (button)
  (magpt--btn--call
   (lambda (_e) (magpt-open-response-patch (button-get button 'magpt-entry)))
   (button-get button 'magpt-entry)))

(defun magpt--btn-check-patch (button)
  (magpt--btn--call
   (lambda (e) (magpt-check-response-patch e))
   (button-get button 'magpt-entry)))

(defun magpt--btn-apply-patch-cached (button)
  "Apply ENTRY's patch to index only (git apply --cached) after confirm."
  (unless magpt-allow-apply-safe-ops
    (user-error "Applying operations is disabled (magpt-allow-apply-safe-ops is nil)"))
  (let* ((e (button-get button 'magpt-entry))
         (resp (plist-get e :response))
         (root (magpt--project-root)))
    (let* ((check (magpt--git-apply-check-temp root resp "--cached"))
           (ok (zerop (car check)))
           (preview (with-temp-buffer
                      (insert (string-trim-right resp) "\n")
                      (buffer-string))))
      (if (not ok)
          (message "magpt: patch --check failed; not applying")
        (when (y-or-n-p "Apply patch to index (git apply --cached)? ")
          (ignore-errors (magpt--git-apply-temp root resp "--cached"))
          (message "magpt: patch applied to index"))))))

(defun magpt--eshell-popup-insert (cmd)
  "Open an eshell popup below and insert CMD at the prompt (do not execute)."
  (require 'eshell)
  (let* ((height (max 8 (min 15 (floor (* 0.3 (window-total-height))))))
         (win (split-window-below height)))
    (select-window win)
    (eshell)
    (goto-char (point-max))
    (when (fboundp 'eshell-kill-input)
      (ignore-errors (eshell-kill-input)))
    (insert (string-trim-right (or cmd "")))
    (message "magpt: command inserted into eshell")))

(defun magpt--btn-eshell-insert (button)
  "Button action: insert the attached command into an eshell popup."
  (let ((cmd (button-get button 'magpt-command)))
    (unless (and (stringp cmd) (> (length cmd) 0))
      (user-error "No command to insert"))
    (magpt--eshell-popup-insert cmd)))

(defun magpt--insert-entry-buttons (entry)
  "Insert action buttons for ENTRY on current line."
  (let* ((task (plist-get entry :task))
         (valid (plist-get entry :valid))
         (start (point)))
    ;; Common actions
    (insert "  ")
    (insert-text-button "[Copy]"
                        'action #'magpt--btn-copy
                        'follow-link t
                        'help-echo "Copy response to kill-ring"
                        'magpt-entry entry)
    (insert "  ")
    (insert-text-button "[JSON]"
                        'action #'magpt--btn-open-json
                        'follow-link t
                        'help-echo "Open response in JSON buffer"
                        'magpt-entry entry)
    ;; Task-specific
    (pcase task
      ('stage-by-intent
       (when (and valid magpt-allow-apply-safe-ops)
         (insert "  ")
         (insert-text-button "[Apply]"
                             'action #'magpt--btn-apply-stage-plan
                             'follow-link t
                             'help-echo "Apply staging plan (file-level)"
                             'magpt-entry entry)))
      ('stage-by-intent-hunks
       (insert "  ")
       (insert-text-button "[Open patch]"
                           'action #'magpt--btn-open-patch
                           'follow-link t
                           'help-echo "Open unified diff patch"
                           'magpt-entry entry)
       (insert "  ")
       (insert-text-button "[Check patch]"
                           'action #'magpt--btn-check-patch
                           'follow-link t
                           'help-echo "git apply --check"
                           'magpt-entry entry)
       (when magpt-allow-apply-safe-ops
         (insert "  ")
         (insert-text-button "[Apply to index]"
                             'action #'magpt--btn-apply-patch-cached
                             'follow-link t
                             'help-echo "git apply --cached (after check)"
                             'magpt-entry entry)))
      ('resolve-conflict-here
       (insert "  ")
       (insert-text-button "[Open patch]"
                           'action #'magpt--btn-open-patch
                           'follow-link t
                           'help-echo "Open suggested conflict resolution patch"
                           'magpt-entry entry)
       (insert "  ")
       (insert-text-button "[Check patch]"
                           'action #'magpt--btn-check-patch
                           'follow-link t
                           'help-echo "Validate patch with git apply --check"
                           'magpt-entry entry))
      ('commit-lint-suggest
       (insert "  ")
       (insert-text-button "[Copy msg]"
                           'action #'magpt--btn-copy-commit-message
                           'follow-link t
                           'help-echo "Copy suggested commit message"
                           'magpt-entry entry)
       (insert "  ")
       (insert-text-button "[Preview msg]"
                           'action #'magpt--btn-preview-commit-message
                           'follow-link t
                           'help-echo "Preview suggested commit message"
                           'magpt-entry entry)
       (insert "  ")
       (insert-text-button "[Insert msg]"
                           'action #'magpt--btn-insert-commit-message
                           'follow-link t
                           'help-echo "Insert into commit buffer (with confirmation)"
                           'magpt-entry entry))
      ('branch-name-suggest
       (insert "  ")
       (insert-text-button "[Copy name]"
                           'action #'magpt--btn-copy-branch-name
                           'follow-link t
                           'help-echo "Copy suggested branch name"
                           'magpt-entry entry)
       (when magpt-allow-apply-safe-ops
         (insert "  ")
         (insert-text-button "[Create branch]"
                             'action #'magpt--btn-create-branch
                             'follow-link t
                             'help-echo "git switch -c <name>"
                             'magpt-entry entry))
       (insert "  ")
       (insert-text-button "[Copy rationale]"
                           'action #'magpt--btn-copy-branch-rationale
                           'follow-link t
                           'help-echo "Copy rationale"
                           'magpt-entry entry)))
    (insert "\n")
    (put-text-property start (point) 'read-only t)))

(defun magpt--history-append-entry (task request response &optional note &rest kvs)
  "Append an entry to history for TASK and refresh Magit AI overview if visible.
Extra KV pairs can be provided in KVS to extend the stored plist."
  (let* ((resp-raw (magpt--response->string (or response "")))
         (resp (magpt--sanitize-response resp-raw))
         (looks-like-json (string-match-p "\\`[ \t\n]*[{\\[]" resp))
         (json-valid
          (and looks-like-json
               (condition-case _err
                   (progn (json-parse-string resp :object-type 'alist) t)
                 (error nil))))
         (entry (list :time (format-time-string "%Y-%m-%d %H:%M:%S")
                      :task task
                      :request (or request "")
                      :response resp
                      :valid json-valid
                      :note note)))
    ;; Extend entry with any extra kvs (e.g., :status-snapshot).
    (when kvs (setq entry (append entry kvs)))
    (push entry magpt--history-entries)
    (magpt--log "history: task=%s json?=%s valid=%s resp-preview=%s"
                task looks-like-json json-valid
                (substring resp 0 (min 180 (length resp))))

    (magpt--refresh-magit-status-visible)))

;;;; Section: Apply infrastructure (Phase 2)
;;
;; Generic helpers for applying results from history. Only safe operations are
;; allowed in Phase 2 (e.g., stage/unstage whole files), gated by
;; `magpt-allow-apply-safe-ops'.

(defun magpt--history-tasks ()
  "Return a list of unique task symbols present in history."
  (delete-dups (mapcar (lambda (e) (plist-get e :task)) magpt--history-entries)))

(defun magpt--history-last-entry-for (task)
  "Return the most recent history entry plist for TASK, or nil if none.
Entries are pushed to the head of `magpt--history-entries', so the first match is the latest."
  (seq-find (lambda (e) (eq (plist-get e :task) task))
            magpt--history-entries))

(defun magpt--entry-ensure-json (entry)
  "Parse and return JSON from ENTRY's response as an alist.
Arrays are returned as lists to simplify iteration.
Signal a user-error if the response is not valid JSON."
  (let ((resp (plist-get entry :response)))
    (condition-case _err
        (json-parse-string (or resp "") :object-type 'alist :array-type 'list)
      (error
       (user-error "Response is not valid JSON for task %s" (plist-get entry :task))))))

;;;###autoload
(defun magpt-apply-last (task)
  "Apply the most recent result for TASK from history.
This command only performs safe, reversible operations and is gated by
`magpt-allow-apply-safe-ops'. Concrete task handlers are enabled in later steps."
  (interactive
   (progn
     (unless magpt-enable-task-registry
       (user-error "Enable `magpt-enable-task-registry' to use apply commands"))
     (let* ((tasks (magpt--history-tasks))
            (names (mapcar (lambda (s) (symbol-name s)) tasks))
            (choice (completing-read "Apply last for task: " names nil t)))
       (list (intern choice)))))
  (unless magpt-allow-apply-safe-ops
    (user-error "Applying operations is disabled (magpt-allow-apply-safe-ops is nil)"))
  (pcase task
    ('stage-by-intent (magpt--apply-stage-by-intent-last))
    (_ (user-error "No apply handler for task: %s" task))))

;;;; Section: Recommend tasks (Phase 2) — Explain Hunk/Region
;;
;; Read-only task that explains a selected region in a file buffer or the current
;; Magit diff hunk under point. No mutations; result is visible in the AI overview.

(defun magpt--ctx-hunk-or-region (_ctx)
  "Return (data preview bytes) for current region in a file buffer or Magit diff hunk.
DATA is a plist with at least :kind and :text; may include :file :start :end."
  (cond
   ;; Region in a file-visiting buffer
   ((and (region-active-p) buffer-file-name)
    (let* ((beg (region-beginning))
           (end (region-end))
           (text (buffer-substring-no-properties beg end))
           (line-beg (line-number-at-pos beg))
           (line-end (line-number-at-pos end))
           (data (list :kind 'region
                       :file (abbreviate-file-name buffer-file-name)
                       :start line-beg :end line-end
                       :text text)))
      (list data text (magpt--string-bytes text))))
   ;; Magit diff hunk under point (heuristic search for @@ headers)
   ((and (derived-mode-p 'magit-diff-mode))
    (save-excursion
      (let (hstart hend)
        (unless (re-search-backward "^@@ " nil t)
          (user-error "Place point inside a diff hunk (header starts with @@)"))
        (setq hstart (line-beginning-position))
        (if (re-search-forward "^@@ \\|^diff --git " nil t)
            (setq hend (line-beginning-position))
          (setq hend (point-max)))
        (let* ((text (buffer-substring-no-properties hstart hend))
               (data (list :kind 'hunk :file "<diff>" :text text)))
          (list data text (magpt--string-bytes text))))))
   (t
    (user-error "Select a region in a file or place point on a Magit diff hunk"))))

(defun magpt--prompt-explain-hunk (data)
  "Build prompt for explaining a code change (region or diff hunk) with DATA."
  (let ((ilang (or magpt-info-language "English"))
        (text (or (plist-get data :text) "")))
    (format (concat
             "Explain the following code change concisely.\n"
             "Return ONLY JSON with fields:\n"
             "  summary: string,\n"
             "  rationale: string,\n"
             "  risks: array of strings\n"
             "Answer STRICTLY in %s for all textual fields. No Markdown outside JSON.\n\n"
             "--- BEGIN CONTEXT ---\n%s\n--- END CONTEXT ---\n")
            ilang
            text)))

(defun magpt--render-explain-hunk (json _data)
  "Append result JSON for 'explain-hunk-region' into history (visible in AI overview)."
  (magpt--history-append-entry 'explain-hunk-region (or magpt--current-request "") (or json "")
                               "JSON: {summary, rationale, risks[]}"))

(defvar magpt--recommend-tasks-registered nil
  "Non-nil when recommend (Phase 2) tasks have been registered.")

(defun magpt--register-recommend-tasks ()
  "Register Phase 2 recommend tasks:
- Explain Hunk/Region (read-only)
- Stage by Intent (groups, file-level apply only)
- Stage by Intent (hunks via unified diff; safe preview/apply to index)
- PR/Range Summary (read-only)"
  (unless magpt--recommend-tasks-registered
    ;; Explain Hunk/Region
    (magpt-register-task
     (magpt--task :name 'explain-hunk-region
                  :title "Explain Hunk/Region"
                  :scope 'file
                  :context-fn #'magpt--ctx-hunk-or-region
                  :prompt-fn  #'magpt--prompt-explain-hunk
                  :render-fn  #'magpt--render-explain-hunk
                  :apply-fn   nil
                  :confirm-send? t))
    ;; Stage by Intent (file-level actions only)
    (magpt-register-task
     (magpt--task :name 'stage-by-intent
                  :title "Stage by Intent (groups)"
                  :scope 'repo
                  :context-fn #'magpt--ctx-stage-intent
                  :prompt-fn  #'magpt--prompt-stage-intent
                  :render-fn  #'magpt--render-stage-intent
                  :apply-fn   nil
                  :confirm-send? t))
    ;; Stage by Intent (hunks via unified diff)
    (magpt-register-task
     (magpt--task :name 'stage-by-intent-hunks
                  :title "Stage by Intent (hunks via patch)"
                  :scope 'repo
                  :context-fn #'magpt--ctx-stage-intent-hunks
                  :prompt-fn  #'magpt--prompt-stage-intent-hunks
                  :render-fn  #'magpt--render-stage-intent-hunks
                  :apply-fn   nil
                  :confirm-send? t))
    ;; PR/Range Summary
    (magpt-register-task
     (magpt--task :name 'range-summary
                  :title "PR/Range Summary"
                  :scope 'repo
                  :context-fn #'magpt--ctx-range-summary
                  :prompt-fn  #'magpt--prompt-range-summary
                  :render-fn  #'magpt--render-range-summary
                  :apply-fn   nil
                  :confirm-send? t))
    (setq magpt--recommend-tasks-registered t)))

;;;###autoload
(defun magpt-explain-hunk-region ()
  "Run 'Explain Hunk/Region' (Phase 2, read-only) and append result to history (see AI overview)."
  (interactive)
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use experimental tasks"))
  (magpt--register-recommend-tasks)
  (magpt-run-task 'explain-hunk-region))

;;;; Section: Recommend tasks (Phase 2) — Stage by Intent (hunks via unified diff)
;;
;; Ask the model to produce a minimal unified diff patch to stage selected hunks.
;; We validate with `git apply --cached --check` and (optionally) apply to index.

(defun magpt--ctx-stage-intent-hunks (_ctx)
  "Collect context for hunk-level staging patch suggestion.
Return (data preview bytes)."
  (let* ((root (magpt--project-root))
         ;; Provide both porcelain and full unstaged diff for better grounding.
         (porc (magpt--git root "status" "--porcelain"))
         (diff (magpt--git root "diff" "--no-color"))
         (preview (format "STATUS:\n%s\n\nDIFF:\n%s"
                          (string-join (seq-take (split-string porc "\n" t) 200) "\n")
                          (if (> (length diff) 8000) (concat (substring diff 0 8000) " …") diff)))
         (bytes (magpt--string-bytes preview)))
    (list (list :porcelain porc :diff diff) preview bytes)))

(defun magpt--prompt-stage-intent-hunks (data)
  "Build prompt for hunk-level staging patch suggestion."
  (let ((ilang (or magpt-info-language "English")))
    (format (concat
             "Produce a minimal unified diff to STAGE only the most coherent hunks.\n"
             "Rules:\n"
             "- Output ONLY a valid unified diff (no prose, no Markdown).\n"
             "- Base is the current working tree; the patch MUST apply with `git apply --cached --check`.\n"
             "- Prefer safe, small steps; do not include unrelated hunks.\n"
             "Answer STRICTLY in %s (only if any textual fields appear; usually none).\n\n"
             "--- BEGIN STATUS ---\n%s\n--- END STATUS ---\n\n"
             "--- BEGIN DIFF ---\n%s\n--- END DIFF ---\n")
            ilang
            (plist-get data :porcelain)
            (plist-get data :diff))))

(defun magpt--render-stage-intent-hunks (patch _data)
  "Append the suggested unified diff PATCH to history (visible in AI overview)."
  (magpt--history-append-entry 'stage-by-intent-hunks (or magpt--current-request "") (or patch "")
                               "Unified diff; check via git apply --cached --check"))

;;;###autoload
(defun magpt-stage-by-intent-hunks ()
  "Request a minimal unified diff to stage selected hunks (safe preview)."
  (interactive)
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use experimental tasks"))
  (magpt--register-recommend-tasks)
  (magpt-run-task 'stage-by-intent-hunks))

;;;; Section: Recommend tasks (Phase 2) — Stage by Intent
;;
;; Suggest logical groups of changes to stage/unstage. Apply is limited to
;; reversible, whole-file operations only (git add / git restore --staged).

(defun magpt--ctx-stage-intent (_ctx)
  "Collect porcelain status for stage-by-intent task.
Return (data preview bytes) where DATA is the raw porcelain for simplicity."
  (let* ((root (magpt--project-root))
         (porc (magpt--git root "status" "--porcelain"))
         (bytes (magpt--string-bytes porc)))
    (list porc porc bytes)))

(defun magpt--prompt-stage-intent (porcelain)
  "Build prompt for stage-by-intent. Answer strictly in `magpt-info-language'."
  (let ((ilang (or magpt-info-language "English")))
    (format (concat
             "Group the following Git changes into a few logical groups for staging.\n"
             "Return ONLY JSON with:\n"
             "  groups: array of { title: string, rationale: string, files: array of { path: string, action: \"stage\"|\"unstage\" } }\n"
             "Constraints: only whole-file actions (no hunks). Prefer minimal, safe steps.\n"
             "Answer STRICTLY in %s for textual fields.\n\n"
             "--- BEGIN PORCELAIN ---\n%s\n--- END PORCELAIN ---\n")
            ilang porcelain)))

(defun magpt--render-stage-intent (json _data)
  "Append result JSON for 'stage-by-intent' into history (visible in AI overview)."
  (magpt--history-append-entry 'stage-by-intent (or magpt--current-request "") (or json "")
                               "JSON: {groups[].files[{path,action:stage|unstage}]}"))

(defun magpt--apply-stage-by-intent-last ()
  "Apply the latest 'stage-by-intent' plan from history.
File-level only; asks for confirmation and shows explicit git commands."
  (interactive)
  (unless magpt-allow-apply-safe-ops
    (user-error "Applying operations is disabled (magpt-allow-apply-safe-ops is nil)"))
  (let* ((e (magpt--history-last-entry-for 'stage-by-intent)))
    (unless e (user-error "No 'stage-by-intent' results in history"))
    (let* ((data (magpt--entry-ensure-json e))
           ;; Be tolerant to older/wrong schema: accept 'group' as fallback.
           (groups (or (alist-get 'groups data)
                       (alist-get 'group data)
                       '()))
           (ops (cl-loop for g in groups append
                         (let ((files (alist-get 'files g)))
                           (cl-loop for f in files
                                    for action = (alist-get 'action f)
                                    for path = (alist-get 'path f)
                                    when (and (member action '("stage" "unstage"))
                                              (stringp path) (> (length path) 0))
                                    collect (cons action path))))))
      (when (null ops) (user-error "No file operations to apply"))
      (let* ((root (magpt--project-root))
             (preview (mapconcat
                       (lambda (op)
                         (pcase op
                           (`("stage" . ,p)   (format "git add -- %s" p))
                           (`("unstage" . ,p) (format "git restore --staged -- %s" p))
                           (_ (format "# skip %S" op))))
                       ops "\n")))
        (when (y-or-n-p (format "Apply file staging plan?\n%s\nProceed? " preview))
          (dolist (op ops)
            (pcase op
              (`("stage" . ,p)   (ignore-errors (magpt--git root "add" "--" p)))
              (`("unstage" . ,p) (ignore-errors (magpt--git root "restore" "--staged" "--" p)))
              (_ nil)))
          (message "magpt: applied staging plan; review in Magit status"))))))

;;;###autoload
(defun magpt-stage-by-intent ()
  "Run 'Stage by Intent' (Phase 2) and append plan to history (read-only; see AI overview)."
  (interactive)
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use experimental tasks"))
  (magpt--register-recommend-tasks)
  (magpt-run-task 'stage-by-intent))

;;;###autoload
(defun magpt-stage-by-intent-apply-last ()
  "Apply latest 'Stage by Intent' plan from history (file-level only)."
  (interactive)
  (magpt--apply-stage-by-intent-last))

;;;; Section: Recommend tasks (Phase 2) — PR/Range Summary
;;
;; Read-only task that summarizes a commit range into a PR/MR description.

(defun magpt--read-range-default ()
  "Read a commit RANGE using Magit if available; otherwise prompt."
  (if (and (featurep 'magit) (fboundp 'magit-read-range))
      (magit-read-range "Range for summary")
    (read-string "Range (e.g., HEAD~5..HEAD): " "HEAD~5..HEAD")))

(defun magpt--ctx-range-summary (range)
  "Collect LOG/STAT for RANGE. Return (data preview bytes)."
  (let* ((root (magpt--project-root))
         (rng (or range (magpt--read-range-default)))
         (log (magpt--git root "log" "--no-color" "--date=short"
                          "--pretty=%h %ad %an %s" rng))
         (stat (magpt--git root "log" "--no-color" "--stat" "--oneline" rng))
         (preview (format "RANGE: %s\n\nLOG:\n%s\n\nSTAT:\n%s"
                          rng
                          (if (> (length log) 4000) (concat (substring log 0 4000) " …") log)
                          (string-join (seq-take (split-string stat "\n" t) 200) "\n")))
         (bytes (magpt--string-bytes preview)))
    (list (list :range rng :log log :stat stat) preview bytes)))

(defun magpt--prompt-range-summary (data)
  "Build prompt for PR/Range Summary using DATA (:range :log :stat)."
  (let ((ilang (or magpt-info-language "English")))
    (format (concat
             "Draft a concise Pull Request description for the given commit range.\n"
             "Return ONLY JSON with fields:\n"
             "  title: string,\n"
             "  summary: string,\n"
             "  highlights: array of strings,\n"
             "  checklist: array of strings\n"
             "Answer STRICTLY in %s.\n\n"
             "--- RANGE ---\n%s\n\n--- LOG ---\n%s\n\n--- STAT ---\n%s\n")
            ilang
            (plist-get data :range)
            (plist-get data :log)
            (plist-get data :stat))))

(defun magpt--render-range-summary (json _data)
  "Append PR/Range Summary JSON into history (visible in AI overview)."
  (magpt--history-append-entry 'range-summary (or magpt--current-request "") (or json "")
                               "JSON: {title, summary, highlights[], checklist[]}"))

;;;###autoload
(defun magpt-range-summary (&optional range)
  "Run 'PR/Range Summary' task and append result to history (read-only; see AI overview)."
  (interactive)
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use experimental tasks"))
  (magpt--register-recommend-tasks)
  (magpt-run-task 'range-summary range))

;;;; Section: Resolve tasks (Phase 3) — Conflicts (preview only)
;;
;; Explain conflict and propose minimal patch. We only show and validate; no apply by default.

(defun magpt--ctx-conflict-buffer (_ctx)
  "Collect current buffer with conflict markers for patch suggestion."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^<<<<<<< " nil t)
      (user-error "No conflict markers in current buffer")))
  (let* ((fname (or buffer-file-name (buffer-name)))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (bytes (magpt--string-bytes text)))
    (list (list :file fname :text text) text bytes)))

(defun magpt--prompt-resolve-conflict (data)
  "Prompt for conflict resolution patch."
  (format (concat
           "You resolve Git merge conflicts. Output ONLY a minimal unified diff patch.\n"
           "Rules:\n- Base path: %s\n- Keep both sides' intent; minimal edits.\n- No prose, only unified diff.\n\n"
           "--- BEGIN FILE WITH CONFLICT ---\n%s\n--- END FILE ---\n")
          (plist-get data :file)
          (plist-get data :text)))

(defun magpt--render-resolve-conflict (patch _data)
  "Render suggested conflict resolution PATCH."
  (magpt--history-append-entry 'resolve-conflict-here (or magpt--current-request "") (or patch "")
                               "Unified diff; preview and validate with git apply --check"))

(defvar magpt--resolve-tasks-registered nil
  "Non-nil when resolve (Phase 3) tasks are registered.")

(defun magpt--register-resolve-tasks ()
  (unless magpt--resolve-tasks-registered
    (magpt-register-task
     (magpt--task :name 'resolve-conflict-here
                  :title "Resolve conflict here (patch suggestion)"
                  :scope 'file
                  :context-fn #'magpt--ctx-conflict-buffer
                  :prompt-fn  #'magpt--prompt-resolve-conflict
                  :render-fn  #'magpt--render-resolve-conflict
                  :apply-fn   nil
                  :confirm-send? t))
    (setq magpt--resolve-tasks-registered t)))

;;;###autoload
(defun magpt-resolve-conflict-here ()
  "Explain and propose a minimal patch for the current conflict (preview only)."
  (interactive)
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use experimental tasks"))
  (magpt--register-resolve-tasks)
  (magpt-run-task 'resolve-conflict-here))

;;;; Section: Assist tasks (Phase 1, read-only)
;;
;; These tasks are safe: they only observe and suggest; no mutations.
;; Results are appended to history and shown in the Magit AI Overview; basic validation hints included.

;; Explain Status

(defcustom magpt-include-magit-keys-in-suggestions t
  "If non-nil, include a dynamic Magit key cheatsheet in Explain Status prompts.
The model is instructed to add a `keys' array to each suggestion with matching key
sequences taken ONLY from the cheatsheet. If no suitable key exists, it should
use an empty list instead of guessing."
  :type 'boolean
  :group 'magpt)

(defun magpt--magit-keys-runtime ()
  "Return an alist of (DESCRIPTION . (KEYS...)) for common Magit actions.
Keys are resolved from `magit-status-mode-map' and Transient suffixes when available."
  (when (and (featurep 'magit) (boundp 'magit-status-mode-map))
    (let* ((map magit-status-mode-map))
      (cl-labels
          ((keys-for (fn)
             (when (fboundp fn)
               (mapcar #'key-description (where-is-internal fn map))))
           (prefix-keys-for (prefix-fn)
             (when (fboundp prefix-fn)
               (mapcar #'key-description (where-is-internal prefix-fn map))))
           (clean-keys (keys)
             (delete-dups
              (seq-filter (lambda (k)
                            (and (stringp k)
                                 (not (string-match-p "<menu-bar" k))))
                          (seq-filter #'identity keys))))
           (composite-keys (prefix-fn target-fn)
             (when (and (featurep 'transient) (fboundp 'transient-get-suffixes))
               (let* ((pkeys (prefix-keys-for prefix-fn))
                      (suffixes (ignore-errors (transient-get-suffixes prefix-fn))))
                 (when (and pkeys suffixes)
                   (cl-loop for suf in suffixes
                            for cmd = (ignore-errors (oref suf command))
                            for skey = (ignore-errors (oref suf key))
                            when (and (eq cmd target-fn)
                                      (stringp skey) (> (length skey) 0))
                            append (mapcar (lambda (pk) (format "%s %s" pk skey)) pkeys)))))))
        (let* ((pairs
                (list
                 (cons "Stage selection"         (keys-for 'magit-stage))
                 (cons "Unstage selection"       (keys-for 'magit-unstage))
                 (cons "Stage all"               (keys-for 'magit-stage-modified))
                 (cons "Unstage all"             (keys-for 'magit-unstage-all))
                 (cons "Commit (create)"
                       (or (keys-for 'magit-commit-create)
                           (composite-keys 'magit-commit 'magit-commit-create)
                           '("c c")))  ;; fallback
                 (cons "Commit (amend)"
                       (or (keys-for 'magit-commit-amend)
                           (composite-keys 'magit-commit 'magit-commit-amend)
                           '("c a")))  ;; fallback
                 (cons "Commit with AI message (magpt)"
                       (or (keys-for 'magpt-commit-staged)
                           (composite-keys 'magit-commit 'magpt-commit-staged)
                           '("c i")))  ;; fallback
                 (cons "Push (to push-remote)"
                       (or (keys-for 'magit-push-current)
                           (keys-for 'magit-push)
                           (composite-keys 'magit-push 'magit-push-current)))
                 (cons "Fetch"
                       (or (keys-for 'magit-fetch)
                           (composite-keys 'magit-fetch 'magit-fetch)))
                 (cons "Pull"
                       (or (keys-for 'magit-pull)
                           (composite-keys 'magit-pull 'magit-pull)))
                 (cons "Switch branch"
                       (or (keys-for 'magit-branch-checkout)
                           (keys-for 'magit-checkout)
                           (composite-keys 'magit-branch 'magit-branch-checkout)))
                 (cons "Create branch"
                       (or (keys-for 'magit-branch-and-checkout)
                           (keys-for 'magit-branch-create)
                           (composite-keys 'magit-branch 'magit-branch-and-checkout)
                           (composite-keys 'magit-branch 'magit-branch-create)))
                 (cons "Log (current)"
                       (or (keys-for 'magit-log-current)
                           (composite-keys 'magit-log 'magit-log-current)))
                 (cons "Diff (working tree)"
                       (or (keys-for 'magit-diff-working-tree)
                           (keys-for 'magit-diff)
                           (composite-keys 'magit-diff 'magit-diff-working-tree)))
                 (cons "Stash"
                       (or (keys-for 'magit-stash)
                           (composite-keys 'magit-stash 'magit-stash)))
                 (cons "Blame"                   (keys-for 'magit-blame))
                 (cons "Rebase"
                       (or (keys-for 'magit-rebase)
                           (composite-keys 'magit-rebase 'magit-rebase)))))
               out)
          (dolist (p pairs)
            (let* ((desc (car p))
                   (ks (clean-keys (cdr p))))
              (when ks
                (push (cons desc ks) out))))
          (nreverse out))))))

(defun magpt--format-magit-keys-cheatsheet ()
  "Format runtime Magit key bindings as a simple cheatsheet string."
  (let ((alist (magpt--magit-keys-runtime)))
    (if (null alist)
        ""
      (mapconcat
       (lambda (pair)
         (format "- %s: %s" (car pair) (mapconcat #'identity (cdr pair) ", ")))
       alist "\n"))))

(defun magpt--format-magit-keys-cheatsheet-safe ()
  "Safe wrapper around `magpt--format-magit-keys-cheatsheet' with timeout and logging."
  (let ((start (float-time)) out)
    (condition-case err
        (with-timeout (0.5 (setq out ""))
          (setq out (or (magpt--format-magit-keys-cheatsheet) "")))
      (error
       (magpt--log "magit-keys-cheatsheet error: %s" (error-message-string err))
       (setq out "")))
    (magpt--log "magit-keys-cheatsheet: dur=%.3fs bytes=%d"
                (- (float-time) start) (length out))
    out))

(defun magpt--porcelain-parse (porc)
  "Parse 'git status --porcelain' into staged/unstaged/untracked file lists."
  (let (staged unstaged untracked)
    (dolist (l (split-string (or porc "") "\n" t))
      (when (string-match "\\`\\([ MADRCU?]\\)\\([ MADRCU?]\\) \\(.*\\)\\'" l)
        (let ((x (match-string 1 l))
              (y (match-string 2 l))
              (path (match-string 3 l)))
          (cond
           ;; Untracked
           ((and (string= x "?") (string= y "?"))
            (push path untracked))
           (t
            ;; Any non-space X means there is something staged for this path.
            (when (not (string= x " "))
              (push path staged))
            ;; Any non-space Y means there is something unstaged in worktree.
            (when (not (string= y " "))
              (push path unstaged)))))))
    (list :staged (nreverse staged)
          :unstaged (nreverse unstaged)
          :untracked (nreverse untracked))))

(defun magpt--ctx-status (_ctx)
  "Collect minimal git status for explain-status task, recent git output, plus Magit keys (optional).
Return (data preview bytes). DATA is a plist with :status, :recent-git-output, optional :magit-keys,
and parsed lists :staged-files, :unstaged-files, :untracked-files."
  (let* ((root (magpt--project-root)))
    (magpt--log "ctx-status: root=%s begin" root)
    ;; Raw porcelain for XY legend (for precise index/worktree interpretation).
    (magpt--log "ctx-status: calling: git status --porcelain")
    (let* ((porc (magpt--git root "status" "--porcelain"))
           (short (string-join (seq-take (split-string porc "\n" t) 200) "\n"))
           (parsed (magpt--porcelain-parse porc))
           (staged   (plist-get parsed :staged))
           (unstaged (plist-get parsed :unstaged))
           (untracked (plist-get parsed :untracked)))
      (magpt--log "ctx-status: porcelain lines=%d" (length (split-string short "\n" t)))
      ;; Additionally update per-repo log: short human-readable status with branches.
      (magpt--log "ctx-status: calling: git status -sb")
      (let* ((_ignore (ignore-errors (magpt--git root "status" "-sb"))))
        (magpt--log "ctx-status: recent-get begin")
        (let* ((recent (magpt--recent-git-output-get root)))
          (magpt--log "ctx-status: recent-get done chars=%d" (length (or recent "")))
          (let* ((t0 (float-time))
                 (lst (split-string (or recent "") "\n" t))
                 (len (length lst))
                 (sz 42)
                 (recent-trunc (string-join (last lst sz) "\n")))
            (magpt--log "ctx-status: recent-trunc done lines=%d kept=%d dur=%.3fs"
                        len (min len sz) (- (float-time) t0))
            (let* ((keys (when magpt-include-magit-keys-in-suggestions
                           (progn
                             (magpt--log "ctx-status: computing Magit keys cheatsheet...")
                             (magpt--format-magit-keys-cheatsheet-safe))))
                   (keys-bytes (length (or keys ""))))
              (when magpt-include-magit-keys-in-suggestions
                (magpt--log "ctx-status: keys computed bytes=%d" keys-bytes))
              (let* ((preview (concat
                               (if (and keys (not (string-empty-p keys)))
                                   (format "STATUS:\n%s\n\nMAGIT KEYS:\n%s"
                                           short
                                           (string-join (seq-take (split-string keys "\n" t) 50) "\n"))
                                 short)
                               (when (> (length recent-trunc) 0)
                                 (concat "\n\nRECENT GIT OUTPUT:\n" recent-trunc))))
                     (bytes (magpt--string-bytes preview)))
                (magpt--log "ctx-status: end root=%s status-lines=%d recent-lines=%d keys?=%s preview-bytes=%d"
                            root
                            (length (split-string short "\n" t))
                            (length (split-string recent-trunc "\n" t))
                            (if (and keys (not (string-empty-p keys))) "t" "nil")
                            bytes)
                (list (list :status short
                            :magit-keys keys
                            :recent-git-output recent-trunc
                            :staged-files staged
                            :unstaged-files unstaged
                            :untracked-files untracked)
                      preview bytes)))))))))

(defun magpt--prompt-explain-status (status-or-data)
  "Build prompt for explain-status. Use `magpt-info-language' for textual fields.
Be sure to consider all messages, errors, hints and warnings present in the 'RECENT GIT OUTPUT' section below."
  (let* ((ilang (or magpt-info-language "English"))
         (status (if (stringp status-or-data)
                     status-or-data
                   (or (plist-get status-or-data :status) "")))
         (recent-output (and (not (stringp status-or-data))
                             (plist-get status-or-data :recent-git-output)))
         (keys   (and (not (stringp status-or-data))
                      (plist-get status-or-data :magit-keys)))
         (staged-files   (and (not (stringp status-or-data))
                              (plist-get status-or-data :staged-files)))
         (unstaged-files (and (not (stringp status-or-data))
                              (plist-get status-or-data :unstaged-files)))
         (untracked-files (and (not (stringp status-or-data))
                               (plist-get status-or-data :untracked-files)))
         (have-keys (and keys (stringp keys) (> (length keys) 0)))
         (keys-block
          (if have-keys
              (concat
               "\nUse ONLY the Magit key bindings listed below for suggestions[].keys.\n"
               "If no suitable action exists, use [].\n"
               "--- BEGIN MAGIT KEYS HELP ---\n" keys "\n--- END MAGIT KEYS HELP ---\n")
            "\nIf relevant Magit key bindings are known, include them in suggestions[].keys; otherwise use [].\n"))
         (facts-block
          (let* ((sf (and (listp staged-files) staged-files))
                 (uf (and (listp unstaged-files) unstaged-files))
                 (uf2 (and (listp untracked-files) untracked-files)))
            (format (concat
                     "Authoritative facts derived from porcelain (do NOT contradict these):\n"
                     "- If a file is in STAGED FILES and NOT in UNSTAGED FILES, it MUST NOT be called 'unstaged', and you MUST NOT suggest 'git add' for it.\n"
                     "- Only files listed in UNSTAGED FILES may need 'git add'.\n\n"
                     "--- BEGIN FACTS ---\n"
                     "STAGED FILES (%d):\n%s\n\n"
                     "UNSTAGED FILES (%d):\n%s\n\n"
                     "UNTRACKED FILES (%d):\n%s\n"
                     "--- END FACTS ---\n")
                    (length sf) (if (and sf (> (length sf) 0)) (string-join sf "\n") "(none)")
                    (length uf) (if (and uf (> (length uf) 0)) (string-join uf "\n") "(none)")
                    (length uf2) (if (and uf2 (> (length uf2) 0)) (string-join uf2 "\n") "(none)"))))
         (recent-block
          (if (and (stringp recent-output) (> (length recent-output) 0))
              (format "\n--- RECENT GIT OUTPUT ---\n%s\n--- END RECENT GIT OUTPUT ---\n" recent-output)
            "")))
    (format (concat
             "You are an experienced developer. Analyze the current repository status and the full output of recent Git commands.\n"
             "Make sure to account for all messages, errors, hints, and warnings present in the RECENT GIT OUTPUT section below.\n"
             "If it suggests actions, shows conflicts, or requires the user to finish an interactive step — tailor your recommendations to that specifically.\n"
             "Answer STRICTLY in %s. Return ONLY JSON with fields:\n"
             "  summary: string,\n"
             "  risks:   array of strings,\n"
             "  suggestions: array of {\n"
             "    title: string,\n"
             "    commands: array of strings,\n"
             "    keys: array of strings  // Magit key sequences to recommend; if unknown — []\n"
             "  }\n"
             "Instructions:\n"
             "- Interpret Git status codes (see legend), but if there is recent command output with errors/hints/pending steps — PRIORITIZE analyzing that output."
             " Provide solutions and warnings based on the actual messages.\n"
             "- For example: editor needs to be closed, pull conflicts, choosing rebase/merge strategy, etc.\n"
             "- Never suggest stage/unstage for changes already staged.\n"
             "- Use the FACTS block below as authoritative truth when deciding whether a file is staged or unstaged.\n"
             "- For a commit with an AI-generated message prefer [c i] if available; otherwise [c c].\n"
             "%s"
             "\n--- BEGIN STATUS ---\n%s\n--- END STATUS ---\n"
             "%s"
             "\n--- GIT PORCELAIN LEGEND ---\n"
             "XY columns (X=index/staged, Y=worktree):\n"
             "  M  file   → staged modify (index changed)\n"
             "   M file   → unstaged modify (worktree changed)\n"
             "  A  file   → staged add\n"
             "  D  file   → staged delete\n"
             "   D file   → unstaged delete\n"
             "  R  old -> new → staged rename\n"
             "  C  old -> new → staged copy\n"
             "  U? or ?U or UU → merge conflict\n"
             "  ?? file   → untracked file\n"
             "--- END GIT PORCELAIN LEGEND ---\n")
            ilang keys-block facts-block status recent-block)))

(defun magpt--render-explain-status (json data)
  (magpt--history-append-entry 'explain-status (or magpt--current-request "") (or json "")
                               "JSON schema: {summary, risks[], suggestions[].commands[], suggestions[].keys[]}"
                               :status-snapshot (or (and (listp data) (plist-get data :status)) "")))

;; Commit Lint/Fix Suggest

(defun magpt--ctx-commit-lint (_ctx)
  "Collect current commit message (top section only) and staged diff.
Return (data preview bytes)."
  (let* ((buf (or (and (magpt--commit-buffer-p) (current-buffer))
                  (magpt--find-commit-buffer))))
    (unless buf
      (user-error "No commit buffer found"))
    (with-current-buffer buf
      (let* ((bounds (magpt--commit-message-boundaries))
             (msg-end (car bounds))
             (msg (string-trim (buffer-substring-no-properties (point-min) msg-end))))
        (when (string-empty-p msg)
          (user-error "Commit message is empty; write something to lint or generate a message first"))
        (let* ((root (magpt--project-root))
               (diff (magpt--staged-diff root))
               (preview (format "MSG:\n%s\n\n--- DIFF (truncated for preview) ---\n%s"
                                msg
                                (if (> (length diff) 2000) (concat (substring diff 0 2000) " …") diff)))
               (bytes (+ (magpt--string-bytes msg) (magpt--string-bytes diff))))
          (list (list :message msg :diff diff) preview bytes))))))

(defun magpt--prompt-commit-lint (data)
  "Build prompt for commit lint task using DATA (:message :diff).
Uses `magpt-commit-language' for suggestion.message and `magpt-info-language' for issues[]."
  (let* ((msg (plist-get data :message))
         (diff (plist-get data :diff))
         (lang-lines
          (concat
           (when (and (stringp magpt-commit-language)
                      (> (length magpt-commit-language) 0))
             (format "Write suggestion.message STRICTLY in %s.\n" magpt-commit-language))
           (when (and (stringp magpt-info-language)
                      (> (length magpt-info-language) 0))
             (format "Use %s for any explanatory text (e.g., issues[] strings). Answer strictly in this language for non-JSON fields.\n" magpt-info-language)))))
    (format (concat
             "You lint Git commit messages against Conventional Commits.\n"
             "Given the current commit message and staged diff, identify issues and suggest a fix.\n"
             "Rules:\n"
             "- Keep title <= 72 chars, imperative mood.\n"
             "- Suggest minimal changes; don't invent ticket IDs.\n"
             "Return ONLY JSON with fields:\n"
             "  status: \"ok\" | \"issues\",\n"
             "  issues: array of strings,\n"
             "  suggestion: { replace: boolean, message: string }\n\n"
             "%s"
             "--- BEGIN MESSAGE ---\n%s\n--- END MESSAGE ---\n\n"
             "--- BEGIN DIFF ---\n%s\n--- END DIFF ---\n")
            lang-lines
            msg
            diff)))

(defun magpt--render-commit-lint (json data)
  (ignore data)
  (magpt--history-append-entry 'commit-lint-suggest (or magpt--current-request "") (or json "")
                               "JSON schema: {status, issues[], suggestion{replace,message}}"))

;; Branch Name Suggest

(defun magpt--ctx-branch-name (_ctx)
  "Collect brief context for branch name suggestion: porcelain status paths."
  (let* ((root (magpt--project-root))
         (porc (magpt--git root "status" "--porcelain"))
         (paths (mapcar (lambda (l) (string-trim (substring l 3)))
                        (seq-filter (lambda (l) (>= (length l) 3))
                                    (split-string porc "\n" t))))
         (preview (string-join (seq-take paths 50) "\n"))
         (bytes (magpt--string-bytes preview)))
    (list (list :paths paths) preview bytes)))

(defun magpt--prompt-branch-name (data)
  "Build prompt for branch-name-suggest task using DATA (:paths)."
  (let ((paths (plist-get data :paths))
        (ilang (or magpt-info-language "English")))
    (format (concat
             "Propose a safe Git branch name in kebab-case for the current work.\n"
             "Constraints:\n"
             "- lowercase, kebab-case, <= 40 chars, [a-z0-9-] only, no trailing dash.\n"
             "- Prefer intent over specifics; no secrets.\n"
             "Use %s for textual fields like rationale. Answer strictly in this language.\n"
             "Return ONLY JSON with fields:\n"
             "  name: string,\n"
             "  alternatives: array of strings,\n"
             "  rationale: string\n\n"
             "--- BEGIN PATHS ---\n%s\n--- END PATHS ---\n")
            ilang
            (string-join paths "\n"))))

(defun magpt--render-branch-name (json _data)
  (magpt--history-append-entry 'branch-name-suggest (or magpt--current-request "") (or json "")
                               "JSON schema: {name, alternatives[], rationale}"))

(defvar magpt--assist-tasks-registered nil
  "Non-nil when assist tasks have been registered in the registry.")

(defun magpt--register-assist-tasks ()
  "Register Phase 1 assist tasks into the registry (read-only tasks)."
  (unless magpt--assist-tasks-registered
    (magpt-register-task
     (magpt--task :name 'explain-status
                  :title "Explain current status"
                  :scope 'repo
                  :context-fn #'magpt--ctx-status
                  :prompt-fn  #'magpt--prompt-explain-status
                  :render-fn  #'magpt--render-explain-status
                  :apply-fn   nil
                  :confirm-send? nil)) ;; Explain Status is safe — no confirmation required
    (magpt-register-task
     (magpt--task :name 'commit-lint-suggest
                  :title "Commit Lint / Fix Suggest"
                  :scope 'repo
                  :context-fn #'magpt--ctx-commit-lint
                  :prompt-fn  #'magpt--prompt-commit-lint
                  :render-fn  #'magpt--render-commit-lint
                  :apply-fn   nil
                  :confirm-send? t))
    (magpt-register-task
     (magpt--task :name 'branch-name-suggest
                  :title "Branch Name Suggest"
                  :scope 'repo
                  :context-fn #'magpt--ctx-branch-name
                  :prompt-fn  #'magpt--prompt-branch-name
                  :render-fn  #'magpt--render-branch-name
                  :apply-fn   nil
                  :confirm-send? t))
    (setq magpt--assist-tasks-registered t)))

(defun magpt--ensure-assist-ready ()
  "Ensure registry is enabled and assist tasks are registered."
  (magpt--log "ensure-assist-ready: begin enable=%S" magpt-enable-task-registry)
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use assist tasks (Phase 1)"))
  (magpt--register-assist-tasks)
  (magpt--log "ensure-assist-ready: done; tasks=%s"
              (mapcar #'symbol-name (magpt--hash-table-keys magpt--tasks))))

;;;###autoload
(defun magpt-explain-status ()
  "Run the 'Explain Status' task and update AI overview in Magit (read-only)."
  (interactive)
  (magpt--log "key [. g]: magpt-explain-status invoked buffer=%s root=%s"
              (buffer-name)
              (ignore-errors (magpt--project-root)))
  (condition-case magpt--e
      (progn
        (magpt--log "explain-status: ensure start")
        (magpt--ensure-assist-ready)
        (magpt--log "explain-status: ensure OK; dispatching run-task")
        (magpt-run-task 'explain-status)
        (magpt--log "explain-status: run-task dispatched"))
    (quit
     ;; Closing/cancelling Transient — keep logs quiet.
     (magpt--log "explain-status: quit (transient closed)"))
    (error
     (let* ((emsg (condition-case _ (error-message-string magpt--e)
                    (error "<no-error-object>")))
            (esym (car-safe magpt--e))
            (edata (cdr-safe magpt--e)))
       (magpt--log "explain-status: ERROR: %s" emsg)
       (magpt--log "explain-status: signal=%S data=%S" esym edata)
       (magpt--log "explain-status: buffer=%s default-directory=%s"
                   (buffer-name) default-directory)
       (magpt--log "explain-status: BT:\n%s" (magpt--backtrace-string))))))
;;;###autoload
(defun magpt-commit-lint-suggest ()
  "Run the 'Commit Lint/Fix Suggest' task and update AI overview in Magit (read-only)."
  (interactive)
  (magpt--ensure-assist-ready)
  (magpt-run-task 'commit-lint-suggest))

;;;###autoload
(defun magpt-branch-name-suggest ()
  "Run the 'Branch Name Suggest' task and update AI overview in Magit (read-only)."
  (interactive)
  (magpt--ensure-assist-ready)
  (magpt-run-task 'branch-name-suggest))

;;;; Section: AI Actions transient (menus built from history)
;;
;; Non-intrusive dynamic menu based on the latest `explain-status` entry in history.
;; No background LLM calls; explicit "g" runs the task.

(defvar magpt--ai-actions-suggestions nil
  "List of suggestion plists from the last explain-status:
(:title STRING :commands STRING :keys LIST-OF-STRINGS).")

(defvar magpt--ai-actions-summary nil
  "Summary string from the last explain-status, if available.")

(defun magpt--ai-suggestions-from-last-explain-status ()
  "Extract suggestions list and summary from the last 'explain-status' history entry."
  (let* ((e (magpt--history-last-entry-for 'explain-status))
         (data (and e (magpt--entry-parse-json-safe e))))
    (when data
      (let ((sugs (or (alist-get 'suggestions data) '()))
            (summary (alist-get 'summary data)))
        (setq magpt--ai-actions-summary (and (stringp summary) summary))
        (mapcar
         (lambda (s)
           (let* ((title (or (alist-get 'title s) ""))
                  (cmds  (mapconcat (lambda (c) (format "%s" c))
                                    (or (alist-get 'commands s) '()) "\n"))
                  (keys  (let ((ks (or (alist-get 'keys s)
                                       (alist-get 'magit_keys s))))
                           (and (listp ks) (seq-filter #'stringp ks)))))
             (list :title title :commands cmds :keys keys)))
         sugs)))))

(defun magpt--ai-actions-init ()
  "Initialize AI actions state from history."
  (setq magpt--ai-actions-suggestions
        (or (magpt--ai-suggestions-from-last-explain-status) '()))
  (let ((n (length magpt--ai-actions-suggestions)))
    (magpt--log "ai-actions-init: suggestions=%d summary?=%s"
                n (if (and (stringp magpt--ai-actions-summary)
                           (> (length magpt--ai-actions-summary) 0))
                      "t" "nil"))
    n))

(defun magpt--ai-actions-choose-index ()
  "Prompt for a suggestion index using completing-read."
  (unless magpt--ai-actions-suggestions (magpt--ai-actions-init))
  (let* ((titles (mapcar (lambda (it) (plist-get it :title))
                         magpt--ai-actions-suggestions))
         (choice (completing-read "Suggestion: " titles nil t)))
    (cl-position choice titles :test #'string=)))

(defun magpt-ai-actions-preview (&optional idx)
  "Preview commands for a suggestion (open read-only buffer with shell-mode)."
  (interactive)
  (magpt--ai-actions-init)
  (if (zerop (length magpt--ai-actions-suggestions))
      (user-error "No suggestions found; run magpt-explain-status first")
    (let* ((i (or idx (magpt--ai-actions-choose-index)))
           (sug (nth i magpt--ai-actions-suggestions))
           (title (plist-get sug :title))
           (cmds (plist-get sug :commands))
           (keys (plist-get sug :keys))
           (keys-str (and (listp keys) (string-join (mapcar (lambda (k) (format "%s" k)) keys) ", ")))
           (body (if keys-str
                     (format "# Magit keys: %s\n\n%s" keys-str cmds)
                   cmds)))
      (magpt--log "ai-actions-preview: idx=%s title=%s keys=%s"
                  i title (or keys-str "[]"))
      (magpt--btn-preview-text (format "AI suggestion: %s" title) body 'shell))))

(defun magpt-ai-actions-copy (&optional idx)
  "Copy commands for a suggestion to the kill-ring."
  (interactive)
  (magpt--ai-actions-init)
  (if (zerop (length magpt--ai-actions-suggestions))
      (user-error "No suggestions found; run magpt-explain-status first")
    (let* ((i (or idx (magpt--ai-actions-choose-index)))
           (sug (nth i magpt--ai-actions-suggestions))
           (cmds (plist-get sug :commands)))
      (kill-new cmds)
      (message "magpt: suggestion commands copied"))))

(defun magpt-ai-actions-copy-summary ()
  "Copy the latest summary to the kill-ring."
  (interactive)
  (unless magpt--ai-actions-summary
    (magpt--ai-actions-init))
  (if (not (and (stringp magpt--ai-actions-summary)
                (> (length magpt--ai-actions-summary) 0)))
      (user-error "No summary available; run magpt-explain-status first")
    (kill-new magpt--ai-actions-summary)
    (message "magpt: summary copied")))

(defun magpt-ai-actions-reload ()
  "Reload AI actions state from the overview and refresh transient UI."
  (interactive)
  (magpt--ai-actions-init)
  (magpt--log "ai-actions-reload: suggestions=%d" (length magpt--ai-actions-suggestions))
  (when (featurep 'transient)
    (transient-setup 'magpt-ai-actions))
  (message "magpt: AI actions reloaded from overview"))

(when (featurep 'transient)
  (transient-define-prefix magpt-ai-actions ()
    "AI actions (from last Explain Status result)"
    [["Suggestions"
      ("p" "Preview suggestion..." magpt-ai-actions-preview)
      ("y" "Copy suggestion..." magpt-ai-actions-copy)
      ("s" "Copy summary" magpt-ai-actions-copy-summary)]
     ["Overview/Tasks"
      ("g" "Get new recommendations (Explain Status)" magpt-explain-status)
      ("r" "Reload from overview" magpt-ai-actions-reload)]]))

(unless (fboundp 'magpt-ai-actions)
  (defun magpt-ai-actions ()
    "Fallback AI actions when `transient' is not available."
    (interactive)
    (magpt--ai-actions-init)
    (call-interactively #'magpt-ai-actions-preview)))

(defun magpt-ai-actions-entry ()
  "Entry point for '.' key in Magit; logs and opens AI actions."
  (interactive)
  (magpt--log "key [.]: magpt-ai-actions-entry buffer=%s root=%s"
              (buffer-name)
              (ignore-errors (magpt--project-root)))
  (if (featurep 'transient)
      (transient-setup 'magpt-ai-actions)
    (magpt-ai-actions)))

;;;; Section: Magit Status integration — AI overview (read-only)
;;
;; Shows the latest Explain Status summary and top suggestions in magit-status.
;; No background LLM calls; data is taken from history.

(defcustom magpt-magit-overview-enabled t
  "If non-nil, insert a compact 'AI overview (magpt)' section into magit-status."
  :type 'boolean
  :group 'magpt)

;; Helper: refresh visible Magit status buffers to update AI overview on new data.
(defun magpt--refresh-magit-status-visible ()
  "Refresh visible Magit status buffers (if any) to update AI overview."
  (when (featurep 'magit)
    (dolist (win (window-list))
      (with-current-buffer (window-buffer win)
        (when (derived-mode-p 'magit-status-mode)
          (ignore-errors
            (cond
             ((fboundp 'magit-refresh) (magit-refresh))
             ((fboundp 'magit-refresh-buffer) (magit-refresh-buffer)))))))))

;;;###;; Mini renderer for Explain Status inside Magit: use child sections so only headings highlight.

(defun magpt--request-extract-status (request)
  "Extract the STATUS block from REQUEST (between --- BEGIN STATUS --- and --- END STATUS ---)."
  (when (stringp request)
    (let* ((beg (regexp-quote "--- BEGIN STATUS ---"))
           (end (regexp-quote "--- END STATUS ---"))
           (re (concat beg "\\(?:\n\\)?\\(\\(?:.\\|\n\\)*?\\)\\(?:\n\\)?" end)))
      (when (string-match re request)
        (string-trim-right (match-string 1 request))))))

(defun magpt--status-lines-equal-p (a b)
  "Return non-nil if porcelain snapshots A and B are equivalent ignoring order and blank lines."
  (let* ((sa (sort (split-string (or (string-trim a) "") "\n" t) #'string<))
         (sb (sort (split-string (or (string-trim b) "") "\n" t) #'string<)))
    (equal sa sb)))

(defun magpt-magit-insert-ai-overview ()
  "Insert a compact 'AI overview (magpt)' section into magit-status."
  (when (and magpt-magit-overview-enabled
             (featurep 'magit)
             (fboundp 'magit-insert-section)
             (fboundp 'magit-insert-heading))
    (magit-insert-section (magit-section 'magpt-ai-overview)
      ;; Parent heading: only this line is highlighted by Magit.
      (magit-insert-heading "AI overview (magpt)")
      (let* ((ex (magpt--history-last-entry-for 'explain-status))
             (cl (magpt--history-last-entry-for 'commit-lint-suggest))
             (bn (magpt--history-last-entry-for 'branch-name-suggest))
             (rc (magpt--history-last-entry-for 'resolve-conflict-here)))
        (if (not ex)
            (insert (format "  %s\n" (magpt--i18n 'overview-no-data)))
          (progn
            (let* ((stale
                    (condition-case _
                        (let* ((snapshot (plist-get ex :status-snapshot))
                               (req (plist-get ex :request))
                               (old (or snapshot
                                        (and (stringp req) (magpt--request-extract-status req))))
                               (root (magpt--project-root))
                               (porc (magpt--git root "status" "--porcelain"))
                               (cur (string-join (seq-take (split-string porc "\n" t) 200) "\n")))
                          (and old (not (magpt--status-lines-equal-p old cur))))
                      (error nil))))
              (when stale
                (insert (format "  %s\n" (magpt--i18n 'overview-stale)))))
            ;; Inline Explain Status: Summary/Risks/Suggestions as child sections of AI overview
            (let* ((data (magpt--entry-parse-json-safe ex))
                   (summary (and data (alist-get 'summary data)))
                   (risks (and data (alist-get 'risks data)))
                   (sugs  (and data (alist-get 'suggestions data)))
                   (compact (eq magpt-ui-density 'compact))
                   (max-r (and compact magpt-overview-compact-max-risks))
                   (max-s (and compact magpt-overview-compact-max-suggestions)))
              ;; Summary subsection
              (when (stringp summary)
                (magit-insert-section (magit-section 'magpt-ai-explain-summary)
                  (magit-insert-heading (magpt--i18n 'overview-summary))
                  (dolist (ln (split-string (string-trim-right summary) "\n"))
                    (insert "  " ln "\n"))))
              ;; Risks subsection
              (when (listp risks)
                (let ((rs (if max-r (seq-take risks max-r) risks)))
                  (magit-insert-section (magit-section 'magpt-ai-explain-risks)
                    (magit-insert-heading (magpt--i18n 'overview-risks))
                    (if rs
                        (dolist (r rs) (insert "  • " (format "%s" r) "\n"))
                      (insert "  • none\n"))
                    (when (and compact (> (length risks) (length rs)))
                      (insert (format "  … %d more\n" (- (length risks) (length rs))))))))
              ;; Suggestions subsection
              (when (listp sugs)
                (let ((ss (if max-s (seq-take sugs max-s) sugs))
                      (i 1))
                  (magit-insert-section (magit-section 'magpt-ai-explain-suggestions)
                    (magit-insert-heading (magpt--i18n 'overview-suggestions))
                    ;; Hide subsection details by default; expand with TAB on the suggestion line
                    (let ((magit-section-initial-visibility-alist
                           (cons (cons 'magpt-ai-suggestion 'hide)
                                 magit-section-initial-visibility-alist)))
                      (dolist (s ss)
                        (let* ((title (or (alist-get 'title s) (format "Suggestion %d" i)))
                               (keys  (or (alist-get 'keys s) (alist-get 'magit_keys s)))
                               (keys-str (and (listp keys)
                                              (string-join (mapcar (lambda (k) (format "%s" k)) keys) ", ")))
                               (cmds (let ((c (alist-get 'commands s)))
                                       (and (listp c) (seq-filter #'stringp c))))
                               (git-cmd (and cmds (seq-find (lambda (c) (string-prefix-p "git " c)) cmds)))
                               (first-cmd (or git-cmd (car cmds))))
                          ;; Subsection per item: heading always visible, details shown when expanded (TAB)
                          (magit-insert-section (magit-section 'magpt-ai-suggestion (list :index i :data s))
                            (magit-insert-heading
                              (format "  %d) %s%s"
                                      i title
                                      (if keys-str (format " [%s]" keys-str) "")))
                            (when (and (stringp first-cmd) (> (length first-cmd) 0))
                              (insert (format "      $ %s\n" first-cmd))
                              (insert "      ")
                              (insert-text-button "[Eshell]"
                                                  'action #'magpt--btn-eshell-insert
                                                  'follow-link t
                                                  'help-echo "Insert command into eshell (bottom popup)"
                                                  'magpt-command first-cmd)
                              (insert "\n")))
                          (setq i (1+ i)))))
                    (when (and compact (> (length sugs) (length ss)))
                      (insert (format "  … %d more (open JSON)\n" (- (length sugs) (length ss)))))))
                ;; Buttons only once for the entire explain-status
                (magpt--insert-entry-buttons ex))
              ;; Fallback if invalid or empty
              (unless (and (plist-get ex :valid) (magpt--entry-parse-json-safe ex))
                (insert (magpt--i18n 'overview-response) "\n")
                (insert (string-trim-right (plist-get ex :response)) "\n\n")
                (magpt--insert-entry-buttons ex)))))
        ;; Child section: Commit Lint / Fix Suggest
        (when cl
          (magit-insert-section (magit-section 'magpt-ai-card-commit-lint)
            (magit-insert-heading "Commit Lint / Fix Suggest")
            (let ((magpt-ui-density 'compact))
              (let ((data (and (plist-get cl :valid) (magpt--entry-parse-json-safe cl))))
                (if data
                    (let* ((status (alist-get 'status data))
                           (issues (or (alist-get 'issues data) '()))
                           (sug (alist-get 'suggestion data))
                           (msg (and sug (alist-get 'message sug))))
                      (insert (format "%s\n" (magpt--i18n 'overview-lint-status)))
                      (insert (or (and (stringp status) status) "(unknown)") "\n\n")
                      (insert (format "%s\n" (magpt--i18n 'overview-issues)))
                      (if (and (listp issues) (> (length issues) 0))
                          (dolist (it issues) (insert "  • " (format "%s" it) "\n"))
                        (insert "  • none\n"))
                      (insert "\n")
                      (insert (format "%s\n" (magpt--i18n 'overview-suggestion)))
                      (if (stringp msg)
                          (progn
                            (dolist (ln (split-string (string-trim-right msg) "\n"))
                              (insert "  " ln "\n"))
                            (insert "\n"))
                        (insert "  (no suggestion message)\n\n")))
                  (insert (magpt--i18n 'overview-response) "\n")
                  (insert (string-trim-right (plist-get cl :response)) "\n\n")))
              (magpt--insert-entry-buttons cl))))
        ;; Child section: Branch Name Suggest
        (when bn
          (magit-insert-section (magit-section 'magpt-ai-card-branch-name)
            (magit-insert-heading "Branch Name Suggest")
            (let ((magpt-ui-density 'compact))
              (let ((data (and (plist-get bn :valid) (magpt--entry-parse-json-safe bn))))
                (if data
                    (let* ((name (alist-get 'name data))
                           (alts (or (alist-get 'alternatives data) '()))
                           (rat  (alist-get 'rationale data)))
                      (insert (format "%s\n" (magpt--i18n 'overview-name)))
                      (insert (or (and (stringp name) name) "(no name)") "\n\n")
                      (insert (format "%s\n" (magpt--i18n 'overview-alternatives)))
                      (if (and (listp alts) (> (length alts) 0))
                          (dolist (n alts) (insert "  • " (format "%s" n) "\n"))
                        (insert "  • none\n"))
                      (insert "\n")
                      (insert (format "%s\n" (magpt--i18n 'overview-rationale)))
                      (if (stringp rat)
                          (progn
                            (dolist (ln (split-string (string-trim-right rat) "\n"))
                              (insert "  " ln "\n"))
                            (insert "\n"))
                        (insert "  (no rationale)\n\n")))
                  (insert (magpt--i18n 'overview-response) "\n")
                  (insert (string-trim-right (plist-get bn :response)) "\n\n")))
              (magpt--insert-entry-buttons bn))))
        ;; Child section: Resolve Conflict (if present)
        (when rc
          (magit-insert-section (magit-section 'magpt-ai-card-resolve-conflict)
            (magit-insert-heading "Resolve Conflict (here)")
            (let ((magpt-ui-density 'compact))
              (insert (magpt--i18n 'overview-response) "\n")
              (insert (string-trim-right (plist-get rc :response)) "\n\n")
              (magpt--insert-entry-buttons rc)))))
      ;; Hint line with key shortcut.
      (insert (propertize "  [.] AI actions\n" 'face 'magpt-badge-info-face))
      (insert "\n"))))

(provide 'magpt)

;;; magpt.el ends here
