;;; magpt.el --- MaGPT: Git/Magit AI assistant via gptel  -*- lexical-binding: t; -*-

;; Author: Peter <11111000000@email.com>
;; URL: https://github.com/11111000000/magpt
;; Version: 1.3.0
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

(defcustom magpt-send-on-empty-context nil
  "If non-nil, still send an LLM request when a task's context is empty (0 bytes).
When nil, magpt skips the request and records a panel entry instead."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-allow-apply-safe-ops t
  "If non-nil, enable safe apply operations (e.g., stage/unstage whole files) from magpt Panel or commands.
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

;;;; Section: i18n helpers (messages for UI and panel)
;;
;; i18n is intentionally minimal and only for user-facing messages, not prompts.

(defun magpt--lang-code ()
  "Return language code symbol based on `magpt-info-language'."
  (let ((l (downcase (or magpt-info-language "english"))))
    (cond
     ((string-match-p "\\`\\(ru\\|рус\\)" l) 'ru)
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
    ;; Panel
    (panel-header . "MaGPT Panel — history (read-only)")
    (panel-note . "Note: %s")
    (panel-request . "Request (preview):")
    (panel-response . "Response:")
    (panel-valid . "Schema/JSON valid: %s")
    (panel-yes . "yes")
    (panel-no . "no or not JSON")
    (panel-actions . "Actions: [Insert disabled] [Apply disabled]")
    (panel-actions-apply-stage-intent . "Actions: [Apply: M-x magpt-stage-by-intent-apply-last]")
    (panel-json-opened . "Panel: opened response in JSON buffer")
    (panel-json-copied . "Panel: response copied to kill-ring")
    (panel-sep . "----------------------------------------")))

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
    ;; Panel
    (panel-header . "MaGPT Панель — история (только чтение)")
    (panel-note . "Заметка: %s")
    (panel-request . "Запрос (превью):")
    (panel-response . "Ответ:")
    (panel-valid . "Схема/JSON валиден: %s")
    (panel-yes . "да")
    (panel-no . "нет или не JSON")
    (panel-actions . "Действия: [Вставка недоступна] [Применение недоступно]")
    (panel-actions-apply-stage-intent . "Действия: [Применить план стейджинга: M-x magpt-stage-by-intent-apply-last]")
    (panel-json-opened . "Панель: ответ открыт в JSON буфере")
    (panel-json-copied . "Панель: ответ скопирован в kill-ring")
    (panel-sep . "----------------------------------------")))

(defun magpt--i18n (key &rest args)
  "Format localized message for KEY with ARGS using `magpt-info-language'."
  (magpt--maybe-load-rc)
  (let* ((lang (magpt--lang-code))
         (tbl (if (eq lang 'ru) magpt--i18n-ru magpt--i18n-en))
         (fmt (or (alist-get key tbl) (alist-get key magpt--i18n-en) "")))
    (magpt--log "i18n: key=%S lang=%S fmt=%s" key lang fmt)
    (if args (apply #'format fmt args) fmt)))

;;;; Section: GPT integration wrappers
;;
;; We centralize gptel-request usage for logging and callback safety. Streaming is opt-in.

(defcustom magpt-stream-output nil
  "If non-nil, request streaming from gptel when supported.
This does not change final insertion semantics."
  :type 'boolean
  :group 'magpt)

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
       (magpt--log "callback exception: %s\nBT:\n%s"
                   (error-message-string err) (magpt--backtrace-string))
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

(defun magpt--process-git (dir &rest args)
  "Execute git with ARGS in DIR. Return (EXIT-CODE . STRING-OUTPUT)."
  (let ((default-directory (file-name-as-directory (or dir default-directory))))
    (with-temp-buffer
      (let ((exit (apply #'process-file (or (magpt--executable-git) "git")
                         nil t nil args))
            (out  (buffer-string)))
        (cons exit (if (string-suffix-p "\n" out) (substring out 0 -1) out))))))

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

(defcustom magpt-progress-spinner nil
  "If non-nil, animate the commit overlay while waiting for the model."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-overlay-spinner-frames
  '("⠁" "⠂" "⠄" "⠂")
  "Frames used for the simple spinner animation."
  :type '(repeat string)
  :group 'magpt)

(defvar-local magpt--commit-overlay nil
  "Overlay shown in the commit buffer while message generation is in progress.")
(defvar-local magpt--overlay-spinner-timer nil
  "Internal timer for overlay spinner animation.")
(defvar-local magpt--overlay-spinner-phase 0
  "Internal spinner phase index for overlay animation.")

(defun magpt--overlay-spinner-on (buf)
  "Start spinner animation in BUF if `magpt-progress-spinner' is enabled."
  (when (and magpt-progress-spinner (buffer-live-p buf))
    (with-current-buffer buf
      (unless (timerp magpt--overlay-spinner-timer)
        (setq magpt--overlay-spinner-phase 0)
        (setq magpt--overlay-spinner-timer
              (run-with-timer
               0 0.12
               (lambda (b)
                 (when (buffer-live-p b)
                   (with-current-buffer b
                     (when (overlayp magpt--commit-overlay)
                       (let* ((frames magpt-overlay-spinner-frames)
                              (i (mod magpt--overlay-spinner-phase (max 1 (length frames))))
                              (frame (nth i frames))
                              (txt (concat frame " " magpt-commit-overlay-text "\n")))
                         (setq magpt--overlay-spinner-phase (1+ magpt--overlay-spinner-phase))
                         (overlay-put magpt--commit-overlay 'before-string
                                      (propertize txt 'face 'magpt-commit-overlay-face)))))))
               buf))))))

(defun magpt--overlay-spinner-off (buf)
  "Stop spinner animation in BUF if running."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (timerp magpt--overlay-spinner-timer)
        (cancel-timer magpt--overlay-spinner-timer)
        (setq magpt--overlay-spinner-timer nil)))))

(defun magpt--show-commit-overlay (buf)
  "Show an overlay in BUF to indicate commit message generation is in progress."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (unless (overlayp magpt--commit-overlay)
          (setq magpt--commit-overlay (make-overlay (point-min) (point-min) buf t t)))
        (overlay-put magpt--commit-overlay 'before-string
                     (propertize (concat magpt-commit-overlay-text "\n")
                                 'face 'magpt-commit-overlay-face))
        (magpt--overlay-spinner-on buf)))))

(defun magpt--remove-commit-overlay (buf)
  "Remove overlay in BUF if present."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (magpt--overlay-spinner-off buf)
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
                   :callback (magpt--safe-callback #'magpt--commit-callback)
                   :stream magpt-stream-output))
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
                         :callback (magpt--safe-callback #'magpt--commit-callback)
                         :stream magpt-stream-output))
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
                                   :callback (magpt--safe-callback #'magpt--commit-callback)
                                   :stream magpt-stream-output))
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

(defcustom magpt-transient-colorize t
  "If non-nil, colorize MaGPT entries in Magit transient menus."
  :type 'boolean
  :group 'magpt)

(defun magpt--transient-desc (s)
  "Return S, optionally propertized with `magpt-transient-face' for transient menus."
  (if magpt-transient-colorize
      (propertize s 'face 'magpt-transient-face)
    s))

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
        ;; Magit dispatch: add Phase 2 entries (Recommend). Appended at end.
        (when (featurep 'transient)
          ;; Append our entries after "!" (Run) in magit-dispatch to avoid transient API issues with nil position.
          (transient-append-suffix 'magit-dispatch "!"
            `("e" ,(magpt--transient-desc "Explain status (magpt)") magpt-explain-status))
          (transient-append-suffix 'magit-dispatch "!"
            `("E" ,(magpt--transient-desc "Explain hunk/region (magpt)") magpt-explain-hunk-region))
          (transient-append-suffix 'magit-dispatch "!"
            `("S" ,(magpt--transient-desc "Stage by intent (magpt)") magpt-stage-by-intent))
          (transient-append-suffix 'magit-dispatch "!"
            `("A" ,(magpt--transient-desc "Apply last stage-by-intent (magpt)") magpt-stage-by-intent-apply-last))
          (transient-append-suffix 'magit-dispatch "!"
            `("R" ,(magpt--transient-desc "Range/PR summary (magpt)") magpt-range-summary))
          (transient-append-suffix 'magit-dispatch "!"
            `("a" ,(magpt--transient-desc "AI actions (magpt)") magpt-ai-actions))))
    (with-eval-after-load 'magit
      (transient-remove-suffix 'magit-commit "i")
      (when (featurep 'transient)
        (transient-remove-suffix 'magit-dispatch "e")
        (transient-remove-suffix 'magit-dispatch "E")
        (transient-remove-suffix 'magit-dispatch "S")
        (transient-remove-suffix 'magit-dispatch "A")
        (transient-remove-suffix 'magit-dispatch "R")
        (transient-remove-suffix 'magit-dispatch "a")))))

;;;; Section: Task registry (experimental core abstraction)
;;
;; A Task encodes a flow: context → prompt → request → render/apply. This is the foundation
;; for the evolving assistant features beyond commit messages. It is optional and off by default.

(defcustom magpt-enable-task-registry t
  "If non-nil, expose experimental task registry commands (assist tasks, panel)."
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
  (pcase-let* ((`(,data ,_preview ,bytes) (funcall (magpt-task-context-fn task) ctx))
               (prompt (funcall (magpt-task-prompt-fn task) data)))
    (if (and (or (null bytes) (zerop bytes))
             (not magpt-send-on-empty-context))
        (let* ((name (magpt-task-name task))
               (note "Empty context; LLM request skipped")
               (resp (if (eq name 'explain-status)
                         "{\"summary\":\"Working tree clean; index empty.\",\"risks\":[],\"suggestions\":[{\"title\":\"Update local repository\",\"commands\":[\"git fetch --all\",\"git pull --ff-only\"]},{\"title\":\"Start a new branch\",\"commands\":[\"git switch -c feature/initial\"]}]}"
                       "{\"note\":\"empty context; nothing to send\"}")))
          (magpt--log "run-task: %s skipped (empty context)" name)
          (magpt--panel-append-entry name (or prompt "") resp note)
          (message "magpt: nothing to send for %s (empty context)" name))
      (let ((ok (if (magpt-task-confirm-send? task)
                    (magpt--confirm-send bytes bytes)
                  t)))
        (when ok
          (let ((gptel-model (or magpt-model gptel-model)))
            (magpt--log "run-task: %s bytes=%d info-lang=%S commit-lang=%S prompt-preview=%s"
                        (magpt-task-name task) (or bytes -1) magpt-info-language magpt-commit-language
                        (let ((n (min 180 (length prompt)))) (substring prompt 0 n)))
            (magpt--gptel-request
             prompt
             :system (magpt--system-prompt 'info)
             :callback
             (lambda (resp info)
               (ignore info)
               (let ((magpt--current-request prompt))
                 (condition-case err
                     (let ((out (string-trim (magpt--response->string resp))))
                       (magpt--log "task-callback: %s resp-type=%S out-preview=%s"
                                   (magpt-task-name task) (type-of resp)
                                   (substring out 0 (min 180 (length out))))
                       (funcall (magpt-task-render-fn task) out data)
                       (when (magpt-task-apply-fn task)
                         (funcall (magpt-task-apply-fn task) out data)))
                   (error
                    (magpt--log "task-callback exception: %s\nBT:\n%s"
                                (error-message-string err) (magpt--backtrace-string))
                    (message "%s" (magpt--i18n 'callback-error (error-message-string err)))))))
             :stream magpt-stream-output)))))))

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
  (let ((task (gethash name magpt--tasks)))
    (unless task (user-error "Unknown magpt task: %s" name))
    (magpt--run-task task ctx)))

;;;; Section: Panel (read-only history UI for tasks)
;;
;; The panel provides a single place to view prompts/responses and quick validity hints.

(defcustom magpt-panel-buffer-name "*magpt-panel*"
  "Name of the magpt panel buffer showing task history."
  :type 'string
  :group 'magpt)

(defcustom magpt-panel-auto-pop t
  "If non-nil, automatically pop to panel when a task finishes."
  :type 'boolean
  :group 'magpt)

(defvar magpt--panel-entries nil
  "List of panel entries (plists):
  :time STRING :task SYMBOL :request STRING :response STRING
  :valid t/nil :note STRING (optional).")

(defvar magpt--current-request nil
  "Dynamically bound prompt/request preview for panel rendering.")

(defun magpt--panel-actions-line (entry)
  "Return a localized actions line for panel ENTRY, reflecting available operations."
  (let* ((task (plist-get entry :task))
         (valid (plist-get entry :valid)))
    (cond
     ((and (eq task 'stage-by-intent) valid magpt-allow-apply-safe-ops)
      (magpt--i18n 'panel-actions-apply-stage-intent))
     (t
      (magpt--i18n 'panel-actions)))))

;; Panel minor mode for navigation and actions
(defvar magpt-panel-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "c") #'magpt-panel-copy-response)
    (define-key m (kbd "j") #'magpt-panel-open-response-json)
    (define-key m (kbd "n") #'magpt-panel-next-entry)
    (define-key m (kbd "p") #'magpt-panel-previous-entry)
    (define-key m (kbd "t") #'magpt-panel-toggle-pretty)
    m)
  "Keymap for `magpt-panel-mode'.")

(define-minor-mode magpt-panel-mode
  "Minor mode for the MaGPT panel buffer.
Keys:
  c — copy current entry response to kill-ring
  j — open current entry response in JSON buffer (pretty-printed when valid)
  n/p — jump to next/previous entry
  t — toggle pretty rendering for supported tasks (e.g., explain-status)"
  :init-value nil :lighter " magpt-panel" :keymap magpt-panel-mode-map
  (read-only-mode (if magpt-panel-mode 1 -1)))

(defcustom magpt-panel-pretty-default t
  "If non-nil, render supported tasks (like explain-status) in a structured, pretty form."
  :type 'boolean
  :group 'magpt)

(defface magpt-panel-title-face
  '((t :inherit mode-line-buffer-id :weight bold))
  "Title face for panel entry header."
  :group 'magpt)

(defface magpt-badge-ok-face
  '((t :inherit success))
  "Badge face for OK/valid markers."
  :group 'magpt)

(defface magpt-badge-warn-face
  '((t :inherit warning))
  "Badge face for warnings/invalid markers."
  :group 'magpt)

(defface magpt-panel-section-face
  '((t :weight bold))
  "Section face for panel structured rendering (e.g., Summary/Risks/Suggestions)."
  :group 'magpt)

(defface magpt-panel-suggestion-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for suggestion titles in pretty rendering."
  :group 'magpt)

(defvar-local magpt--panel-pretty nil
  "Buffer-local toggle for pretty rendering in panel.")

(defun magpt--insert-badge (label face)
  "Insert a bracketed LABEL with FACE."
  (insert (propertize (format "[%s]" label) 'face face) " "))

(defun magpt--panel-insert-header (entry)
  "Insert a styled header for ENTRY with badges."
  (let ((ts (plist-get entry :time))
        (task (plist-get entry :task))
        (valid (plist-get entry :valid)))
    (insert (propertize (format "=== %s — %s ===\n" ts task)
                        'face 'magpt-panel-title-face))
    (magpt--insert-badge (if valid "JSON OK" "JSON?") (if valid 'magpt-badge-ok-face 'magpt-badge-warn-face))
    (insert "\n")))

(defun magpt--panel-parse-json-safe (entry)
  "Parse ENTRY's :response as JSON; return alist or nil."
  (let ((resp (plist-get entry :response)))
    (condition-case _err
        (json-parse-string (or resp "") :object-type 'alist :array-type 'list)
      (error nil))))

(defun magpt--btn-copy-summary (button)
  "Copy explain-status summary."
  (let* ((entry (button-get button 'magpt-entry))
         (data (magpt--panel-parse-json-safe entry))
         (sum (alist-get 'summary data)))
    (when (stringp sum)
      (kill-new sum)
      (message "%s" (magpt--i18n 'panel-json-copied)))))

(defun magpt--btn-copy-suggestion-commands (button)
  "Copy commands for a suggestion."
  (let* ((cmds (button-get button 'magpt-suggestion-commands)))
    (when (stringp cmds)
      (kill-new cmds)
      (message "%s" (magpt--i18n 'panel-json-copied)))))

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

(defun magpt--btn-preview-suggestion-commands (button)
  "Preview commands for a suggestion in a read-only buffer."
  (let* ((idx (button-get button 'magpt-suggestion-index))
         (cmds (button-get button 'magpt-suggestion-commands)))
    (magpt--btn-preview-text (format "Suggestion %s commands" (or idx "?"))
                             cmds 'shell)))

(defun magpt--panel-render-explain-status-pretty (entry)
  "Pretty rendering for explain-status ENTRY: summary, risks, suggestions."
  (let* ((note (or (plist-get entry :note) ""))
         (data (magpt--panel-parse-json-safe entry))
         (summary (and (alist-get 'summary data) (format "%s" (alist-get 'summary data))))
         (risks (or (alist-get 'risks data) '()))
         (sugs (or (alist-get 'suggestions data) '())))
    (when (and (stringp note) (> (length note) 0))
      (insert (format (concat (magpt--i18n 'panel-note) "\n") note)))
    ;; Summary
    (insert (propertize (format "%s\n" "Summary:") 'face 'magpt-panel-section-face))
    (insert (or summary "(no summary)") "\n\n")
    ;; Risks
    (insert (propertize (format "%s\n" "Risks:") 'face 'magpt-panel-section-face))
    (if (and (listp risks) (> (length risks) 0))
        (dolist (r risks) (insert "  • " (format "%s" r) "\n"))
      (insert "  • none\n"))
    (insert "\n")
    ;; Suggestions
    (insert (propertize (format "%s\n" "Suggestions:") 'face 'magpt-panel-section-face))
    (if (and (listp sugs) (> (length sugs) 0))
        (cl-loop for s in sugs
                 for i from 1 do
                 (let* ((title (or (alist-get 'title s) (format "Suggestion %d" i)))
                        (cmds  (mapconcat (lambda (c) (format "%s" c))
                                          (or (alist-get 'commands s) '()) "\n")))
                   (insert (propertize (format "%d) %s\n" i title) 'face 'magpt-panel-suggestion-face))
                   (insert "    ")
                   (make-text-button "[Copy cmds]" nil
                                     'action #'magpt--btn-copy-suggestion-commands
                                     'follow-link t
                                     'help-echo "Copy suggested commands"
                                     'magpt-entry entry
                                     'magpt-suggestion-index i
                                     'magpt-suggestion-commands cmds)
                   (insert "  ")
                   (make-text-button "[Preview cmds]" nil
                                     'action #'magpt--btn-preview-suggestion-commands
                                     'follow-link t
                                     'help-echo "Preview suggested commands"
                                     'magpt-entry entry
                                     'magpt-suggestion-index i
                                     'magpt-suggestion-commands cmds)
                   (insert "\n")))
      (insert "  (no suggestions)\n"))
    (insert "\n")))

(defun magpt-panel-toggle-pretty ()
  "Toggle pretty rendering for supported tasks and refresh the panel."
  (interactive)
  (setq magpt--panel-pretty (not magpt--panel-pretty))
  (magpt-show-panel))

(defun magpt-panel-next-entry ()
  "Move point to the start of the next panel entry."
  (interactive)
  (let ((pos (next-single-property-change (point) 'magpt-entry nil (point-max))))
    (when (and pos (< pos (point-max)))
      (goto-char pos)
      (beginning-of-line))))

(defun magpt-panel-previous-entry ()
  "Move point to the start of the previous panel entry."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'magpt-entry nil (point-min))))
    (when (and pos (> pos (point-min)))
      ;; Jump to the beginning of this entry (property becomes non-nil just after boundary)
      (goto-char (1- pos))
      (setq pos (previous-single-property-change (point) 'magpt-entry nil (point-min)))
      (when pos (goto-char pos)))
    (beginning-of-line)))

(defun magpt--panel-entry-at-point ()
  "Return the panel ENTRY plist at point, or nil if none.
Relies on 'magpt-entry text property."
  (let ((pos (point)) entry)
    (setq entry (get-text-property pos 'magpt-entry))
    (while (and (null entry) (> pos (point-min)))
      (setq pos (1- (or (previous-single-property-change pos 'magpt-entry nil (point-min))
                        (point-min))))
      (setq entry (get-text-property pos 'magpt-entry)))
    entry))

(defun magpt-panel-copy-response ()
  "Copy current entry's response to the kill-ring."
  (interactive)
  (let* ((e (or (magpt--panel-entry-at-point) (car (last magpt--panel-entries)))))
    (unless e (user-error "No panel entry at point"))
    (let ((resp (plist-get e :response)))
      (kill-new resp)
      (message "%s" (magpt--i18n 'panel-json-copied)))))

(defun magpt-panel-open-response-json ()
  "Open current entry's response in a JSON buffer and pretty-print when valid."
  (interactive)
  (let* ((e (or (magpt--panel-entry-at-point) (car (last magpt--panel-entries)))))
    (unless e (user-error "No panel entry at point"))
    (let* ((resp (plist-get e :response))
           (buf (get-buffer-create "*magpt-json*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (string-trim-right resp) "\n")
          (goto-char (point-min))
          (when (fboundp 'json-mode) (ignore-errors (json-mode)))
          (condition-case _err
              (progn
                ;; Try to pretty-print; falls back silently if invalid JSON.
                (when (fboundp 'json-pretty-print-buffer)
                  (json-pretty-print-buffer)))
            (error nil))
          (setq buffer-read-only t)))
      (pop-to-buffer buf)
      (message "%s" (magpt--i18n 'panel-json-opened)))))

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
         (message "%s" (magpt--i18n 'panel-json-copied))))
     e)))

(defun magpt--btn-open-json (button)
  (let ((e (button-get button 'magpt-entry)))
    (magpt--btn--call
     (lambda (_entry) (magpt-panel-open-response-json))
     e)))

(defun magpt--apply-stage-by-intent-entry (entry)
  "Apply stage/unstage plan from ENTRY (stage-by-intent)."
  (let* ((data (magpt--panel-ensure-json entry))
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

(defun magpt-panel-open-response-patch (&optional entry)
  "Open ENTRY's response (or current entry) in a diff-mode buffer."
  (interactive)
  (let* ((e (or entry (magpt--panel-entry-at-point)))
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
    (message "%s" (magpt--i18n 'panel-json-opened))))

(defun magpt-panel-check-response-patch (&optional entry &rest args)
  "Run 'git apply --check' for ENTRY's response patch in index or worktree (per ARGS)."
  (interactive)
  (let* ((e (or entry (magpt--panel-entry-at-point)))
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
   (lambda (_e) (magpt-panel-open-response-patch (button-get button 'magpt-entry)))
   (button-get button 'magpt-entry)))

(defun magpt--btn-check-patch (button)
  (magpt--btn--call
   (lambda (e) (magpt-panel-check-response-patch e))
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

(defun magpt--panel-insert-buttons (entry)
  "Insert action buttons for ENTRY on current line."
  (let* ((task (plist-get entry :task))
         (valid (plist-get entry :valid))
         (start (point)))
    ;; Common actions
    (insert "  ")
    (make-text-button "[Copy]" nil
                      'action #'magpt--btn-copy
                      'follow-link t
                      'help-echo "Copy response to kill-ring"
                      'magpt-entry entry)
    (insert "  ")
    (make-text-button "[JSON]" nil
                      'action #'magpt--btn-open-json
                      'follow-link t
                      'help-echo "Open response in JSON buffer"
                      'magpt-entry entry)
    ;; Task-specific
    (pcase task
      ('stage-by-intent
       (when (and valid magpt-allow-apply-safe-ops)
         (insert "  ")
         (make-text-button "[Apply]" nil
                           'action #'magpt--btn-apply-stage-plan
                           'follow-link t
                           'help-echo "Apply staging plan (file-level)"
                           'magpt-entry entry)))
      ('stage-by-intent-hunks
       (insert "  ")
       (make-text-button "[Open patch]" nil
                         'action #'magpt--btn-open-patch
                         'follow-link t
                         'help-echo "Open unified diff patch"
                         'magpt-entry entry)
       (insert "  ")
       (make-text-button "[Check patch]" nil
                         'action #'magpt--btn-check-patch
                         'follow-link t
                         'help-echo "git apply --check"
                         'magpt-entry entry)
       (when magpt-allow-apply-safe-ops
         (insert "  ")
         (make-text-button "[Apply to index]" nil
                           'action #'magpt--btn-apply-patch-cached
                           'follow-link t
                           'help-echo "git apply --cached (after check)"
                           'magpt-entry entry)))
      ('resolve-conflict-here
       (insert "  ")
       (make-text-button "[Open patch]" nil
                         'action #'magpt--btn-open-patch
                         'follow-link t
                         'help-echo "Open suggested conflict resolution patch"
                         'magpt-entry entry)
       (insert "  ")
       (make-text-button "[Check patch]" nil
                         'action #'magpt--btn-check-patch
                         'follow-link t
                         'help-echo "Validate patch with git apply --check"
                         'magpt-entry entry)))
    (insert "\n")
    (put-text-property start (point) 'read-only t)))

(defun magpt--panel-buffer ()
  "Return the panel buffer, rendering all entries (pretty when available)."
  (let ((buf (get-buffer-create magpt-panel-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n\n" (magpt--i18n 'panel-header)))
        (unless (local-variable-p 'magpt--panel-pretty)
          (setq-local magpt--panel-pretty magpt-panel-pretty-default))
        (dolist (e (reverse magpt--panel-entries))
          (let* ((start (point))
                 (task (plist-get e :task))
                 (req  (plist-get e :request))
                 (resp (plist-get e :response))
                 (valid (plist-get e :valid)))
            ;; Header with badges
            (magpt--panel-insert-header e)
            ;; Request preview (trimmed)
            (insert (format "%s\n%s\n\n"
                            (magpt--i18n 'panel-request)
                            (if (> (length req) 2000) (concat (substring req 0 2000) " …") req)))
            ;; Response
            (cond
             ((and magpt--panel-pretty
                   (eq task 'explain-status)
                   valid
                   (magpt--panel-parse-json-safe e))
              (magpt--panel-render-explain-status-pretty e))
             (t
              (insert (magpt--i18n 'panel-response) "\n")
              (insert (string-trim-right resp) "\n\n")))
            ;; Validity + actions
            (insert (format "%s\n"
                            (format (magpt--i18n 'panel-valid)
                                    (if valid (magpt--i18n 'panel-yes) (magpt--i18n 'panel-no)))))
            (let ((actions (magpt--panel-actions-line e)))
              (insert actions "\n"))
            (magpt--panel-insert-buttons e)
            (insert (magpt--i18n 'panel-sep) "\n\n")
            (let ((end (point)))
              (put-text-property start end 'magpt-entry e)))))
      (magpt-panel-mode 1)
      (goto-char (point-min)))
    buf))

;;;###autoload
(defun magpt-show-panel ()
  "Show the magpt panel with task history."
  (interactive)
  (pop-to-buffer (magpt--panel-buffer)))

(defun magpt--panel-append-entry (task request response &optional note)
  "Append a history entry to panel for TASK with REQUEST/RESPONSE."
  (let* ((resp (magpt--response->string (or response "")))
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
    (push entry magpt--panel-entries)
    (magpt--log "panel: task=%s json?=%s valid=%s resp-preview=%s"
                task looks-like-json json-valid
                (substring resp 0 (min 180 (length resp))))
    (when magpt-panel-auto-pop
      (magpt-show-panel))))

;;;; Section: Apply infrastructure (Phase 2)
;;
;; Generic helpers for applying results from the panel. Only safe operations are
;; allowed in Phase 2 (e.g., stage/unstage whole files), gated by
;; `magpt-allow-apply-safe-ops'.

(defun magpt--panel-tasks-in-history ()
  "Return a list of unique task symbols present in the panel history."
  (delete-dups (mapcar (lambda (e) (plist-get e :task)) magpt--panel-entries)))

(defun magpt--panel-last-entry-for (task)
  "Return the most recent panel entry plist for TASK, or nil if none.
Entries are pushed to the head of `magpt--panel-entries', so the first match is the latest."
  (seq-find (lambda (e) (eq (plist-get e :task) task))
            magpt--panel-entries))

(defun magpt--panel-ensure-json (entry)
  "Parse and return JSON from panel ENTRY's response as an alist.
Arrays are returned as lists to simplify iteration.
Signal a user-error if the response is not valid JSON."
  (let ((resp (plist-get entry :response)))
    (condition-case _err
        (json-parse-string (or resp "") :object-type 'alist :array-type 'list)
      (error
       (user-error "Response is not valid JSON for task %s" (plist-get entry :task))))))

;;;###autoload
(defun magpt-apply-last (task)
  "Apply the most recent result for TASK from the panel.
This command only performs safe, reversible operations and is gated by
`magpt-allow-apply-safe-ops'. Concrete task handlers are enabled in later steps."
  (interactive
   (progn
     (unless magpt-enable-task-registry
       (user-error "Enable `magpt-enable-task-registry' to use apply commands"))
     (let* ((tasks (magpt--panel-tasks-in-history))
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
;; Magit diff hunk under point. No mutations; result is shown in the panel.

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
  "Render result JSON for 'explain-hunk-region' into the panel."
  (magpt--panel-append-entry 'explain-hunk-region (or magpt--current-request "") (or json "")
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
  "Run 'Explain Hunk/Region' (Phase 2, read-only) and show result in *magpt-panel*."
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
  "Render the suggested unified diff PATCH to the panel."
  (magpt--panel-append-entry 'stage-by-intent-hunks (or magpt--current-request "") (or patch "")
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
  "Render result JSON for 'stage-by-intent' into the panel."
  (magpt--panel-append-entry 'stage-by-intent (or magpt--current-request "") (or json "")
                             "JSON: {groups[].files[{path,action:stage|unstage}]}"))

(defun magpt--apply-stage-by-intent-last ()
  "Apply the latest 'stage-by-intent' plan from the panel.
File-level only; asks for confirmation and shows explicit git commands."
  (interactive)
  (unless magpt-allow-apply-safe-ops
    (user-error "Applying operations is disabled (magpt-allow-apply-safe-ops is nil)"))
  (let* ((e (magpt--panel-last-entry-for 'stage-by-intent)))
    (unless e (user-error "No 'stage-by-intent' results in panel"))
    (let* ((data (magpt--panel-ensure-json e))
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
  "Run 'Stage by Intent' (Phase 2) and show plan in *magpt-panel* (read-only)."
  (interactive)
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use experimental tasks"))
  (magpt--register-recommend-tasks)
  (magpt-run-task 'stage-by-intent))

;;;###autoload
(defun magpt-stage-by-intent-apply-last ()
  "Apply latest 'Stage by Intent' plan from the panel (file-level only)."
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
  "Render PR/Range Summary JSON into the panel."
  (magpt--panel-append-entry 'range-summary (or magpt--current-request "") (or json "")
                             "JSON: {title, summary, highlights[], checklist[]}"))

;;;###autoload
(defun magpt-range-summary (&optional range)
  "Run 'PR/Range Summary' task and show result in *magpt-panel* (read-only)."
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
  (magpt--panel-append-entry 'resolve-conflict-here (or magpt--current-request "") (or patch "")
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
;; They use the panel to display results and basic validation hints.

(defun magpt--render-to-panel (task out data)
  "Render OUT/DATA for TASK into the panel (read-only)."
  (ignore data)
  (magpt--panel-append-entry task (or magpt--current-request "") (or out "")))

;; Explain Status

(defun magpt--ctx-status (_ctx)
  "Collect minimal git status for explain-status task.
Return (data preview bytes)."
  (let* ((root (magpt--project-root))
         (porc (magpt--git root "status" "--porcelain"))
         (short (string-join (seq-take (split-string porc "\n" t) 200) "\n"))
         (bytes (magpt--string-bytes short)))
    (list short short bytes)))

(defun magpt--prompt-explain-status (status)
  "Build prompt for explain-status task. Use `magpt-info-language' for textual fields."
  (format (concat
           "You are a senior developer. Review the current Git status and propose next safe actions.\n"
           "Return ONLY JSON with fields:\n"
           "  summary: string,\n"
           "  risks:   array of strings,\n"
           "  suggestions: array of { title: string, commands: array of strings }\n"
           "Answer STRICTLY in %s for all textual fields. No Markdown or extra prose outside JSON.\n\n"
           "--- BEGIN STATUS ---\n%s\n--- END STATUS ---\n")
          (or magpt-info-language "English")
          status))

(defun magpt--render-explain-status (json _data)
  (magpt--panel-append-entry 'explain-status (or magpt--current-request "") (or json "")
                             "JSON schema: {summary, risks[], suggestions[].commands[]}"))

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
  (magpt--panel-append-entry 'commit-lint-suggest (or magpt--current-request "") (or json "")
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
  (magpt--panel-append-entry 'branch-name-suggest (or magpt--current-request "") (or json "")
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
                  :confirm-send? t))
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
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use assist tasks (Phase 1)"))
  (magpt--register-assist-tasks))

;;;###autoload
(defun magpt-explain-status ()
  "Run the 'Explain Status' task and show result in *magpt-panel* (read-only)."
  (interactive)
  (magpt--ensure-assist-ready)
  (magpt-run-task 'explain-status))

;;;###autoload
(defun magpt-commit-lint-suggest ()
  "Run the 'Commit Lint/Fix Suggest' task and show result in *magpt-panel* (read-only)."
  (interactive)
  (magpt--ensure-assist-ready)
  (magpt-run-task 'commit-lint-suggest))

;;;###autoload
(defun magpt-branch-name-suggest ()
  "Run the 'Branch Name Suggest' task and show result in *magpt-panel* (read-only)."
  (interactive)
  (magpt--ensure-assist-ready)
  (magpt-run-task 'branch-name-suggest))

;;;; Section: AI Actions transient (menus built from panel)
;;
;; Non-intrusive dynamic menu based on the latest `explain-status` entry in the Panel.
;; No background LLM calls; explicit "g" runs the task.

(defvar magpt--ai-actions-suggestions nil
  "List of suggestion plists from the last explain-status: (:title STRING :commands STRING).")

(defvar magpt--ai-actions-summary nil
  "Summary string from the last explain-status, if available.")

(defun magpt--ai-suggestions-from-last-explain-status ()
  "Extract suggestions list and summary from the last 'explain-status' panel entry."
  (let* ((e (magpt--panel-last-entry-for 'explain-status))
         (data (and e (magpt--panel-parse-json-safe e))))
    (when data
      (let ((sugs (or (alist-get 'suggestions data) '()))
            (summary (alist-get 'summary data)))
        (setq magpt--ai-actions-summary (and (stringp summary) summary))
        (mapcar (lambda (s)
                  (let* ((title (or (alist-get 'title s) ""))
                         (cmds  (mapconcat (lambda (c) (format "%s" c))
                                           (or (alist-get 'commands s) '()) "\n")))
                    (list :title title :commands cmds)))
                sugs)))))

(defun magpt--ai-actions-init ()
  "Initialize AI actions state from panel."
  (setq magpt--ai-actions-suggestions
        (or (magpt--ai-suggestions-from-last-explain-status) '()))
  (length magpt--ai-actions-suggestions))

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
           (cmds (plist-get sug :commands)))
      (magpt--btn-preview-text (format "AI suggestion: %s" title) cmds 'shell))))

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
  "Reload AI actions state from the panel and refresh transient UI."
  (interactive)
  (magpt--ai-actions-init)
  (when (featurep 'transient)
    (transient-setup 'magpt-ai-actions))
  (message "magpt: AI actions reloaded from panel"))

(when (featurep 'transient)
  (transient-define-prefix magpt-ai-actions ()
    "AI actions (from last Explain Status result)"
    [["Suggestions"
      ("p" "Preview suggestion..." magpt-ai-actions-preview)
      ("y" "Copy suggestion..." magpt-ai-actions-copy)
      ("s" "Copy summary" magpt-ai-actions-copy-summary)]
     ["Panel/Tasks"
      ("o" "Open panel" magpt-show-panel)
      ("g" "Get new recommendations (Explain Status)" magpt-explain-status)
      ("r" "Reload from panel" magpt-ai-actions-reload)]]))

(unless (fboundp 'magpt-ai-actions)
  (defun magpt-ai-actions ()
    "Fallback AI actions when `transient' is not available."
    (interactive)
    (magpt--ai-actions-init)
    (call-interactively #'magpt-ai-actions-preview)))

(provide 'magpt)

;;; magpt.el ends here
