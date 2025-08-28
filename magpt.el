;;; magpt.el --- Generate commit messages from staged diff via gptel + Magit  -*- lexical-binding: t; -*-

;; Author: Peter <11111000000@email.com>
;; URL: https://github.com/11111000000/magpt
;; Version: 1.2.0
;; Package-Requires: ((emacs "28.1") (gptel "0.9"))
;; Keywords: tools, vc, git, ai

;;; Commentary:
;;
;; magpt is an Emacs package that helps write commit messages
;; based on the diff of staged changes using gptel (LLM providers)
;; and integrates with Magit when available.
;;
;; Main features:
;; - Get the diff of all staged changes in the current Git project.
;; - Build a prompt (=magpt-commit-prompt' + diff).
;; - Send an asynchronous request to an LLM via =gptel-request'.
;; - Insert the result into the commit message buffer (git-commit-mode)
;;   or show it in a separate buffer.
;;
;; Architectural notes:
;; - Internal functions (with the magpt-- prefix) strive to be pure: they
;;   take arguments and return values without side effects.
;; - Side effects (buffer/window/message operations) occur only
;;   in interactive commands and callbacks.
;; - Magit is an optional dependency; if available, it's used for
;;   determining the repo root. There are fallbacks to vc/project.el.
;;
;; Usage:
;; - Call the command:
;;     M-x magpt-generate-commit-message
;; - Customize variables in the =magpt' group if needed.

;;; Code:

(require 'gptel)
(require 'vc)
(require 'project)
(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'magit nil t) ;; опционально

;; Simple passthrough wrapper to keep a single call site
(defun magpt--gptel-request (prompt &rest args)
  "Call `gptel-request' with PROMPT and ARGS.
Wrap callback safely and add diagnostic logging."
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

(defun magpt--safe-callback (cb)
  "Wrap CB so that any error is caught and reported safely (with diagnostics)."
  (lambda (resp info)
    (when magpt-log-enabled
      (let* ((ty (type-of resp))
             (preview (condition-case _
                          (let* ((s (magpt--response->string resp))
                                 (n (min 180 (length s))))
                            (substring s 0 n))
                        (error "<unprintable>"))))
        (magpt--log "safe-callback: resp-type=%S preview=%s info=%S"
                    ty preview (and (listp info) (cl-subseq info 0 (min 10 (length info)))))))
    (condition-case err
        (funcall cb resp info)
      (error
       (magpt--log "callback exception: %s\nBT:\n%s"
                   (error-message-string err) (magpt--backtrace-string))
       (message "%s" (magpt--i18n 'callback-error (error-message-string err)))))))

(defgroup magpt nil
  "Generate commit messages from staged diff using gptel."
  :group 'tools
  :group 'vc
  :prefix "magpt-")

(defcustom magpt-model nil
  "Name of the LLM model for gptel, or nil.
If nil, uses the default model/provider configured in gptel.
Example: \"gpt-4o-mini\" or any other model available in your gptel setup."
  :type '(choice (const :tag "Use gptel default model" nil)
                 (string :tag "Explicitly specify a model"))
  :group 'magpt)

(defcustom magpt-info-language "English"
  "Preferred natural language for informational content produced by the model in assist tasks.
Examples: status summaries, rationales, and risk lists. This does not translate Emacs UI strings,
it only nudges the model via prompts to use this language for textual fields."
  :type 'string
  :group 'magpt)

(defcustom magpt-commit-language nil
  "Preferred natural language for generated commit messages.
When non-nil, magpt will instruct the model to produce commit messages in this language."
  :type '(choice (const :tag "No preference" nil)
                 (string :tag "Language name"))
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
  "Prompt template, to be appended with the staged diff.
The final prompt will be this string plus a separator and the diff."
  :type 'string
  :group 'magpt)

(defcustom magpt-max-diff-bytes 200000
  "Maximum size in bytes for the diff included in the request.
If nil, don't limit. If the diff exceeds this limit, it will be truncated,
and a truncation notice will be added."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Max bytes"))
  :group 'magpt)

(defcustom magpt-insert-into-commit-buffer t
  "If t, the result will be inserted into the commit message buffer (git-commit-mode) if available.
Otherwise, the result will be shown in a separate *magpt-commit* buffer."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-project-root-strategy 'prefer-magit
  "Strategy to determine the Git project root for getting the diff.
- prefer-magit: Magit first, then VC, then project.el, then default-directory (with check).
- prefer-vc: VC first, then Magit, then project.el, then default-directory (with check).
- prefer-project: project.el first, then Magit, then VC, then default-directory (with check)."
  :type '(choice
          (const :tag "Prefer Magit" prefer-magit)
          (const :tag "Prefer VC" prefer-vc)
          (const :tag "Prefer project.el" prefer-project))
  :group 'magpt)

(defcustom magpt-diff-args '("--staged" "--no-color")
  "Additional arguments for the git diff command.
By default, staged diff without color is used."
  :type '(repeat string)
  :group 'magpt)

(defcustom magpt-confirm-before-send t
  "If non-nil, ask for confirmation before sending the staged diff to the LLM.
The prompt shows the size in bytes (and notes if truncation will apply)."
  :type 'boolean
  :group 'magpt)

;;;; Project rc (.magptrc) support

(defcustom magpt-rc-file-name ".magptrc"
  "Per-project RC file name located at the project root.
If present, its settings override all magpt variables (highest priority)."
  :type 'string
  :group 'magpt)

(defvar magpt--rc-state nil
  "Internal cache of project rc: plist (:path PATH :mtime TIME :data ALIST).")

(defun magpt--locate-rc ()
  "Return absolute path to project .magptrc if found, else nil."
  (let ((root (ignore-errors (magpt--project-root))))
    (when root
      (let ((f (expand-file-name magpt-rc-file-name root)))
        (when (file-exists-p f) f)))))

(defun magpt--read-rc (file)
  "Read FILE and return an alist of (SYMBOL . VALUE). Does not eval arbitrary code.
Supports both ( (var . val) ... ) and '( (var . val) ... ) forms."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((sexp (read (current-buffer))))
          ;; Strip leading quote if present: '( ... ) -> ( ... )
          (when (and (listp sexp) (eq (car-safe sexp) 'quote))
            (setq sexp (cadr sexp)))
          (when (and (listp sexp) (consp sexp))
            sexp)))
    (error
     (magpt--log "rc read error: %s: %s" file (error-message-string err))
     nil)))

(defun magpt--apply-rc (alist)
  "Apply ALIST of (SYMBOL . VALUE) to magpt variables. Highest priority."
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

(defun magpt--maybe-load-rc ()
  "Load and apply project .magptrc if present and changed. Overrides user options."
  (let ((f (magpt--locate-rc)))
    (when f
      (let* ((attr (file-attributes f))
             (mtime (when attr (file-attribute-modification-time attr))))
        (when (or (null magpt--rc-state)
                  (not (equal (plist-get magpt--rc-state :path) f))
                  (not (equal (plist-get magpt--rc-state :mtime) mtime)))
          (let ((alist (magpt--read-rc f)))
            (setq magpt--rc-state (list :path f :mtime mtime :data alist))
            (magpt--apply-rc alist)
            (magpt--log "rc loaded: %s keys=%s"
                        f (mapcar (lambda (kv)
                                    (cond
                                     ((consp kv) (symbol-name (car kv)))
                                     ((symbolp kv) (symbol-name kv))
                                     (t (format "%S" kv))))
                                  (or alist '())))))))))

;;;; logging helpers

(defcustom magpt-log-enabled t
  "If non-nil, write diagnostic messages into `magpt-log-buffer-name'."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-log-buffer-name "*magpt-log*"
  "Name of the buffer used for diagnostic logs."
  :type 'string
  :group 'magpt)

(defun magpt--log (fmt &rest args)
  "Append a diagnostic line to `magpt-log-buffer-name' and echo minimal info.
Robust against bad format strings and mismatched args."
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

;;;; i18n helpers

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
    (panel-sep . "----------------------------------------")))

(defun magpt--i18n (key &rest args)
  "Format localized message for KEY with ARGS using `magpt-info-language'."
  ;; Ensure RC is applied (for language) and log selected language.
  (magpt--maybe-load-rc)
  (let* ((lang (magpt--lang-code))
         (tbl (if (eq lang 'ru) magpt--i18n-ru magpt--i18n-en))
         (fmt (or (alist-get key tbl) (alist-get key magpt--i18n-en) "")))
    (magpt--log "i18n: key=%S lang=%S fmt=%s" key lang fmt)
    (if args
        (apply #'format fmt args)
      fmt)))

(defun magpt--response->string (resp)
  "Return RESP as a string, tolerating non-string structures from backends."
  (cond
   ((stringp resp) resp)
   ;; Common alist shape with content
   ((and (listp resp) (assq 'content resp))
    (let ((c (cdr (assq 'content resp))))
      (if (stringp c) c (format "%S" resp))))
   ;; Try JSON-encode alists/plists/hash-tables
   ((hash-table-p resp)
    (condition-case _ (json-encode resp) (error (format "%S" resp))))
   ((listp resp)
    (condition-case _ (json-encode resp) (error (format "%S" resp))))
   (t (format "%S" resp))))

(defun magpt--system-prompt (kind)
  "Return strict system directive for language. KIND is 'commit or 'info."
  (let* ((lang (pcase kind
                 ('commit magpt-commit-language)
                 (_      magpt-info-language)))
         (l (and (stringp lang) (> (length lang) 0) lang)))
    (when l
      (format "Answer STRICTLY in %s. Do not use any other language." l))))

;;;; Internal helper functions (pure, no side effects)

(defun magpt--executable-git ()
  "Return the path to the git executable or nil."
  (executable-find "git"))

(defun magpt--process-git (dir &rest args)
  "Execute git with ARGS in the DIR directory.
Return cons (EXIT-CODE . STRING-OUTPUT). Doesn't signal errors itself."
  (let ((default-directory (file-name-as-directory (or dir default-directory))))
    (with-temp-buffer
      (let ((exit (apply #'process-file (or (magpt--executable-git) "git")
                         nil t nil args))
            (out  (buffer-string)))
        (cons exit (if (string-suffix-p "\n" out) (substring out 0 -1) out))))))

(defun magpt--git (dir &rest args)
  "Execute git ARGS in DIR and return the output string.
If the command exits non-zero, signal `user-error' with error text."
  (pcase (apply #'magpt--process-git dir args)
    (`(,exit . ,out)
     (if (zerop exit)
         out
       (user-error "Git error (%s): %s" exit out)))))

(defun magpt--git-root-from (dir)
  "Try to get the Git repo root directory for DIR.
Return the root path as a string, or nil if not found."
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
Signal error if no repository is found. Always returns an absolute, normalized directory."
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
  "Return the diff string for staged changes in ROOT.
Uses `magpt-diff-args' over 'git diff'."
  (apply #'magpt--git root "diff" (or magpt-diff-args '("--staged" "--no-color"))))

(defun magpt--truncate-to-bytes (s max-bytes)
  "Return the longest prefix of S whose UTF-8 byte size does not exceed MAX-BYTES.
Uses binary search on character count to ensure UTF-8 boundary."
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
  "Maybe truncate string S to MAX-BYTES bytes.
Returns cons (TRUNCATED-OR-ORIGINAL . TRUNCATEDP)."
  (if (or (null max-bytes) (<= (string-bytes s) max-bytes))
      (cons s nil)
    (let ((tstr (magpt--truncate-to-bytes s max-bytes)))
      (cons tstr t))))

(defun magpt--build-commit-prompt (template diff &optional truncatedp)
  "Build the final prompt for the LLM.
TEMPLATE is the prompt template (string), DIFF is the diff string.
If TRUNCATEDP is non-nil, append a note about the truncated diff.
Respects `magpt-commit-language' by adding a strict directive when non-nil."
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
  "Confirm sending the diff given sizes ORIG-BYTES and SEND-BYTES.
Returns non-nil to proceed."
  (if (not magpt-confirm-before-send)
      t
    (let ((msg (if (= orig-bytes send-bytes)
                   (magpt--i18n 'confirm-send-full send-bytes)
                 (magpt--i18n 'confirm-send-trunc orig-bytes send-bytes))))
      (magpt--log "confirm-send: info-lang=%S msg=%s" magpt-info-language msg)
      (y-or-n-p msg))))

(defun magpt--commit-buffer-p (&optional buf)
  "Return t if BUF appears to be a commit message buffer.
Heuristics:
- major-mode derives from git-commit-mode, or
- file name is COMMIT_EDITMSG, or
- with-editor-mode is enabled (editor buffer for commit message)."
  (with-current-buffer (or buf (current-buffer))
    (or (derived-mode-p 'git-commit-mode)
        (and buffer-file-name
             (string-equal (file-name-nondirectory buffer-file-name)
                           "COMMIT_EDITMSG"))
        (bound-and-true-p with-editor-mode))))

(defun magpt--find-commit-buffer ()
  "Find the active commit message buffer, if any."
  (catch 'found
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (magpt--commit-buffer-p buf))
        (throw 'found buf)))
    nil))

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

(defcustom magpt-stream-output nil
  "If non-nil, request streaming from gptel when supported.
This is experimental and does not change insertion behavior."
  :type 'boolean
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
  "Show an overlay in the BUF to indicate commit message generation in progress."
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

(defun magpt--comment-line-regexp ()
  "Return a regexp that matches comment lines in a Git commit buffer."
  (let* ((c (or (and (boundp 'git-commit-comment-char) git-commit-comment-char)
                (and (boundp 'comment-start)
                     (stringp comment-start)
                     (> (length comment-start) 0)
                     (aref comment-start 0))
                ?#)))
    (format "^%c.*$" c)))

(defun magpt--commit-comment-char ()
  "Return the comment character used by git-commit, or fallback '#'."
  (or (and (boundp 'git-commit-comment-char) git-commit-comment-char)
      (and (boundp 'comment-start)
           (stringp comment-start)
           (> (length comment-start) 0)
           (aref comment-start 0))
      ?#))

(defun magpt--commit-message-boundaries ()
  "Return a cons (MSG-END . COMMENTS-BEG) for current commit buffer.
Detects a trailing comments block (preceded by a blank line) that consists only
of comment lines or blank lines until end of buffer. If found, MSG-END is the
buffer position before that block; otherwise MSG-END is `point-max'."
  (save-excursion
    (goto-char (point-min))
    (let* ((c (magpt--commit-comment-char))
           (rx (format "^%c" c))
           (cand nil)
           pos)
      (while (and (not cand)
                  (setq pos (re-search-forward rx nil t)))
        (let ((bol (match-beginning 0)))
          ;; Require a blank line (or BOB) before the comments block candidate.
          (when (save-excursion
                  (goto-char bol)
                  (forward-line -1)
                  (or (bobp) (looking-at-p "^[ \t]*$")))
            ;; Verify: from BOL to EOB only comment lines or blanks.
            (let ((ok t))
              (save-excursion
                (goto-char bol)
                (while (and ok (not (eobp)))
                  (cond
                   ((looking-at-p rx))          ; comment line
                   ((looking-at-p "^[ \t]*$"))  ; blank line
                   (t (setq ok nil)))
                  (forward-line 1)))
              (when ok (setq cand bol))))))
      (if cand
          (cons cand cand)
        (cons (point-max) nil)))))

(defun magpt--insert-into-commit-buffer (text)
  "Insert TEXT into the commit message buffer, asking for confirmation if needed.
Preserves comment lines at the bottom. Inserts the generated message at the top.
Returns t if insertion is performed; nil otherwise."
  (let ((target (or (and (magpt--commit-buffer-p) (current-buffer))
                    (magpt--find-commit-buffer))))
    (when target
      (magpt--insert-into-commit-buffer-target target text))))

(defun magpt--show-in-output-buffer (text)
  "Show TEXT in the *magpt-commit* buffer and copy to kill-ring."
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

;;;; Публичная команда

;;;###autoload
(defun magpt-generate-commit-message ()
  "Generate a commit message from the staged diff of the current project using gptel.
Behavior:
- If 'magpt-insert-into-commit-buffer' is t and a git-commit-mode buffer is available,
  the result will be inserted into that buffer (with confirmation if there is an existing message).
- Otherwise, the result is displayed in a separate /magpt-commit/ buffer and also copied to the kill-ring."
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



(defun magpt--insert-into-commit-buffer-target (buf text)
  "Insert TEXT into the specified commit message buffer BUF.
Preserves comment lines (lines starting with the Git comment char) at the bottom
Inserts the generated message at the top. Returns t if insertion is performed; nil otherwise."
  (when (and (buffer-live-p buf))
    (with-current-buffer buf
      (when (magpt--commit-buffer-p)
        (let* ((bounds (magpt--commit-message-boundaries))
               (msg-end (car bounds))
               (existing (string-trim (buffer-substring-no-properties (point-min) msg-end))))
          (when (or (string-empty-p existing)
                    (y-or-n-p (magpt--i18n 'replace-current-commit-msg?)))
            (let ((inhibit-read-only t))
              ;; Only remove the current message (up to comments), keep comments section intact.
              (delete-region (point-min) msg-end)
              (goto-char (point-min))
              (insert (string-trim-right text) "\n")
              (goto-char (point-min)))
            (message "%s" (format (magpt--i18n 'inserted-into-buffer-named) (buffer-name buf)))
            t))))))

(defun magpt--commit-callback (response info)
  "Callback for gptel. Inserts or displays the generated commit message.
Prefers inserting into the commit buffer when `magpt-insert-into-commit-buffer' is non-nil
and a suitable buffer is available (taken from :context or searched via `magpt--find-commit-buffer').
Otherwise shows the result in *magpt-commit* and copies it to the kill-ring."
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
  "Start a Magit commit and insert a generated message for staged changes.
If already in a commit message buffer, reuse the current buffer instead of opening a new one.
The generated message will be inserted into that buffer even if you switch windows while waiting.

Opens a commit message buffer when needed, inserts the generated text, and allows the user to
complete the commit with the standard C-c C-c command. This command does not
actually perform the commit.

Requires Magit to be installed."
  (interactive)
  ;; Если команда вызвана из transient-меню Magit, сперва аккуратно его закрываем,
  ;; чтобы post/pre-command хуки transient не падали на nil prefix.
  (when (and (featurep 'transient)
             (boundp 'transient--prefix)
             (bound-and-true-p transient--prefix)
             (fboundp 'transient-quit-all))
    (transient-quit-all))
  ;; Основную работу выполняем на следующем тике, вне контекста transient.
  (run-at-time 0 nil #'magpt--commit-staged-run))

(defun magpt--commit-staged-run ()
  "Implementation helper for `magpt-commit-staged' that actually performs the work."
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
            ;; Уже в commit-буфере: спрашиваем подтверждение, затем показываем overlay и запускаем генерацию.
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
          ;; Не в буфере коммита: откроем его и отправим по готовности.
          (if (magpt--confirm-send orig-bytes send-bytes)
              (let (hook-fn remove-timer)
                (setq hook-fn
                      (lambda ()
                        ;; Одноразовый хук — сразу снимаем по входу в commit-буфер.
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
                ;; На случай отмены коммита — очистим хук через таймаут.
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

;;;###autoload
(define-minor-mode magpt-mode
  "Global minor mode to integrate MagPT AI commit message generation with Magit.
When enabled, adds a [i] transient command to the Magit commit popup to use AI-based message suggestion."
  :global t
  :group 'magpt
  (if magpt-mode
      ;; On enable: Add magpt-commit-staged button to Magit commit transient.
      (with-eval-after-load 'magit
        (transient-append-suffix 'magit-commit "c"
          '("i" "Commit with AI message (magpt)" magpt-commit-staged)))
    ;; On disable: Remove our binding, if present.
    (with-eval-after-load 'magit
      (transient-remove-suffix 'magit-commit "i"))))

;; Experimental task registry (Phase 0). Behind flags; does not affect 1.0.0 behavior.

(defcustom magpt-enable-task-registry nil
  "If non-nil, expose experimental task registry commands.
When disabled, registry APIs remain available but are not bound or invoked."
  :type 'boolean
  :group 'magpt)

(defvar magpt--tasks (make-hash-table :test 'eq)
  "Registry of magpt tasks keyed by symbol.")

(cl-defstruct (magpt-task (:constructor magpt--task))
  name title scope context-fn prompt-fn render-fn apply-fn confirm-send?)

(defun magpt--hash-table-keys (ht)
  "Return a list of keys in hash-table HT."
  (let (ks) (maphash (lambda (k _v) (push k ks)) ht) (nreverse ks)))

(defun magpt-register-task (task)
  "Register TASK (a `magpt-task' struct) in the registry."
  (puthash (magpt-task-name task) task magpt--tasks))

(defun magpt--run-task (task &optional ctx)
  "Run TASK: collect context, build prompt, request model, render/apply.
CTX is passed to the task's context function. Experimental."
  (pcase-let* ((`(,data ,_preview ,bytes) (funcall (magpt-task-context-fn task) ctx))
               (prompt (funcall (magpt-task-prompt-fn task) data))
               (ok (if (magpt-task-confirm-send? task)
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
         :stream magpt-stream-output)))))

;;;###autoload
(defun magpt-run-task (name &optional ctx)
  "Interactively run a registered magpt task NAME. Experimental."
  (interactive
   (progn
     (unless magpt-enable-task-registry
       (user-error "Enable `magpt-enable-task-registry' to use experimental tasks"))
     (magpt--register-assist-tasks)
     (list (intern (completing-read
                    "magpt task: "
                    (mapcar #'symbol-name (magpt--hash-table-keys magpt--tasks)))))))
  ;; Ensure tasks are registered when called programmatically too.
  (magpt--maybe-load-rc)
  (when magpt-enable-task-registry
    (magpt--register-assist-tasks))
  (let ((task (gethash name magpt--tasks)))
    (unless task (user-error "Unknown magpt task: %s" name))
    (magpt--run-task task ctx)))

;;;; Phase 1 — Assist (read-only tasks) and panel

(defcustom magpt-panel-buffer-name "*magpt-panel*"
  "Name of the magpt panel buffer that shows task history and results."
  :type 'string
  :group 'magpt)

(defcustom magpt-panel-auto-pop t
  "If non-nil, automatically pop to panel when a task finishes."
  :type 'boolean
  :group 'magpt)

(defvar magpt--panel-entries nil
  "List of panel entries. Each entry is a plist:
  :time STRING, :task SYMBOL, :request STRING, :response STRING,
  :valid t/nil (for JSON validation), :note STRING (optional).")

(defvar magpt--current-request nil
  "Dynamically bound prompt/request preview for panel rendering.")

(defun magpt--panel-buffer ()
  "Return the buffer for the magpt panel (create if needed)."
  (let ((buf (get-buffer-create magpt-panel-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n\n" (magpt--i18n 'panel-header)))
        (dolist (e (reverse magpt--panel-entries))
          (let ((ts   (plist-get e :time))
                (task (plist-get e :task))
                (req  (plist-get e :request))
                (resp (plist-get e :response))
                (valid (plist-get e :valid))
                (note (or (plist-get e :note) "")))
            (insert (format "=== %s — %s ===\n" ts task))
            (when (and (stringp note) (> (length note) 0))
              (insert (format (concat (magpt--i18n 'panel-note) "\n") note)))
            (insert (format "%s\n%s\n\n"
                            (magpt--i18n 'panel-request)
                            (if (> (length req) 2000) (concat (substring req 0 2000) " …") req)))
            (insert (magpt--i18n 'panel-response) "\n")
            (insert (string-trim-right resp) "\n")
            (insert (format "\n%s\n"
                            (format (magpt--i18n 'panel-valid)
                                    (if valid (magpt--i18n 'panel-yes) (magpt--i18n 'panel-no)))))
            (insert (magpt--i18n 'panel-actions) "\n")
            (insert (magpt--i18n 'panel-sep) "\n\n"))))
      (read-only-mode 1)
      (goto-char (point-min)))
    buf))

;;;###autoload
(defun magpt-show-panel ()
  "Show the magpt panel with history."
  (interactive)
  (pop-to-buffer (magpt--panel-buffer)))

(defun magpt--panel-append-entry (task request response &optional note)
  "Append a history entry to panel for TASK with REQUEST and RESPONSE."
  (let* ((resp (magpt--response->string (or response "")))
         (looks-like-json (string-match-p "\\`[ \t\n]*[{\\[]"
                                          resp))
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

(defun magpt--string-bytes (s)
  "Return UTF-8 bytes of string S."
  (if (stringp s) (string-bytes s) 0))

;;;; Assist tasks (read-only): explain-status, commit-lint-suggest, branch-name-suggest

;; Common renderer to panel
(defun magpt--render-to-panel (task out data)
  "Render OUT and DATA for TASK into the panel buffer (read-only)."
  (ignore data)
  (magpt--panel-append-entry task (or magpt--current-request "") (or out "")))

;; Task: Explain Status

(defun magpt--ctx-status (_ctx)
  "Collect minimal git status for explain-status task.
Returns (data preview bytes)."
  (let* ((root (magpt--project-root))
         (porc (magpt--git root "status" "--porcelain"))
         (short (string-join (seq-take (split-string porc "\n" t) 200) "\n"))
         (bytes (magpt--string-bytes short)))
    (list short short bytes)))

(defun magpt--prompt-explain-status (status)
  "Build prompt for explain-status task. Force output language via `magpt-info-language'."
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

;; Task: Commit Lint/Fix Suggest (read-only)

(defun magpt--ctx-commit-lint (_ctx)
  "Collect current commit message (top section only) and staged diff.
Returns (data preview bytes)."
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
  "Build prompt for commit lint task using DATA plist (:message :diff).
Respects `magpt-commit-language' for suggestion.message and `magpt-info-language' for explanatory text."
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

;; Task: Branch Name Suggest (read-only)

(defun magpt--ctx-branch-name (_ctx)
  "Collect brief context for branch name suggestion: porcelain status."
  (let* ((root (magpt--project-root))
         (porc (magpt--git root "status" "--porcelain"))
         (paths (mapcar (lambda (l) (string-trim (substring l 3)))
                        (seq-filter (lambda (l) (>= (length l) 3))
                                    (split-string porc "\n" t))))
         (preview (string-join (seq-take paths 50) "\n"))
         (bytes (magpt--string-bytes preview)))
    (list (list :paths paths) preview bytes)))

(defun magpt--prompt-branch-name (data)
  "Build prompt for branch-name-suggest task using DATA (:paths).
Hints the model to use `magpt-info-language' for rationale."
  (let ((paths (plist-get data :paths))
        (ilang (or magpt-info-language "English")))
    (format (concat
             "Propose a safe Git branch name in kebab-case for the current work.\n"
             "Base it on the changed paths/themes. Constraints:\n"
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

;; Registration / wrappers

(defvar magpt--assist-tasks-registered nil
  "Non-nil when Phase 1 assist tasks have been registered.")

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

(provide 'magpt)

;;; magpt.el ends here
