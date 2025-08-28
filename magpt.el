;;; magpt.el --- Generate commit messages from staged diff via gptel + Magit  -*- lexical-binding: t; -*-

;; Author: Peter <11111000000@email.com>
;; URL: https://github.com/11111000000/magpt
;; Version: 0.1.0
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
(eval-when-compile (require 'subr-x))
(require 'magit nil t) ;; опционально

(defgroup magpt nil
  "Generate commit messages from staged diff using gptel."
  :group 'tools
  :group 'vc
  :prefix "magpt-")

(defcustom magpt-model "gpt-4.1"
  "Name of the LLM model for gptel, or nil.
If nil, uses the default model/provider configured in gptel.
Example: \"gpt-4o-mini\" or any other model available in your gptel setup."
  :type '(choice (const :tag "Use gptel default model" nil)
                 (string :tag "Explicitly specify a model"))
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
Signal error if no repository is found."
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
      (user-error "No Git repository found for current directory"))
    root))

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
If TRUNCATEDP is non-nil, append a note about the truncated diff."
  (concat
   (string-trim-right (or template ""))
   "\n\n--- BEGIN DIFF ---\n"
   diff
   (when truncatedp "\n\n[diff truncated due to size limit]")
   "\n--- END DIFF ---\n"))

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

(defvar-local magpt--commit-overlay nil
  "Overlay shown in the commit buffer while message generation is in progress.")

(defun magpt--show-commit-overlay (buf)
  "Show an overlay in the BUF to indicate commit message generation in progress."
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

(defun magpt--insert-into-commit-buffer (text)
  "Insert TEXT into the commit message buffer, asking for confirmation if needed.
Preserves comment lines (lines starting with #) at the bottom of the buffer.
Inserts the generated message at the top. Returns t if insertion is performed; nil otherwise."
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
                    (y-or-n-p "Replace the current commit message and insert the generated one? "))
            (let ((inhibit-read-only t))
              ;; Only remove the current message (up to comments), keep comments.
              (delete-region (point-min) msg-end)
              (goto-char (point-min))
              (insert (string-trim-right text) "\n")
              (goto-char (point-min)))
            (message "magpt: commit message inserted into %s" (buffer-name target))
            t))))))

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
    (message "magpt: result copied to kill-ring and shown in *magpt-commit*")))

;;;; Публичная команда

;;;###autoload
(defun magpt-generate-commit-message ()
  "Generate a commit message from the staged diff of the current project using gptel.
Behavior:
- If 'magpt-insert-into-commit-buffer' is t and a git-commit-mode buffer is available,
  the result will be inserted into that buffer (with confirmation if there is an existing message).
- Otherwise, the result is displayed in a separate /magpt-commit/ buffer and also copied to the kill-ring."
  (interactive)
  (unless (magpt--executable-git)
    (user-error "Could not find executable 'git' in PATH"))
  (let* ((target (when magpt-insert-into-commit-buffer
                   (or (and (magpt--commit-buffer-p) (current-buffer))
                       (magpt--find-commit-buffer))))
         (root (magpt--project-root))
         (diff (magpt--staged-diff root)))
    (when (string-empty-p (string-trim diff))
      (user-error "No staged changes found (git add ...)"))
    (let* ((trunc (magpt--maybe-truncate diff magpt-max-diff-bytes))
           (diff (car trunc))
           (truncatedp (cdr trunc))
           (prompt (magpt--build-commit-prompt magpt-commit-prompt diff truncatedp))
           (callback
            (lambda (response info)
              (condition-case err
                  (progn
                    (when target (magpt--remove-commit-overlay target))
                    (let ((errstr (plist-get info :error)))
                      (if errstr
                          (message "magpt/gptel error: %s" errstr)
                        (let ((text (string-trim (or response ""))))
                          (if (string-empty-p text)
                              (message "magpt: empty response from model")
                            (if (and magpt-insert-into-commit-buffer
                                     (magpt--insert-into-commit-buffer text))
                                (message "magpt: commit message inserted")
                              (magpt--show-in-output-buffer text)))))))
                (error
                 (when target (magpt--remove-commit-overlay target))
                 (message "magpt: error in callback: %s" (error-message-string err)))))))
      (when target (magpt--show-commit-overlay target))
      (message "magpt: requesting LLM to generate commit message...")
      (condition-case err
          (let ((gptel-model (or magpt-model gptel-model)))
            (gptel-request prompt :callback callback))
        (error
         (when target (magpt--remove-commit-overlay target))
         (message "magpt: error calling gptel: %s" (error-message-string err)))))))



(defun magpt--insert-into-commit-buffer-target (buf text)
  "Insert TEXT into the specified commit message buffer BUF.
Preserves comment lines (lines starting with #) at the bottom of the buffer.
Inserts the generated message at the top. Returns t if insertion is performed; nil otherwise."
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
                    (y-or-n-p "Replace the current commit message and insert the generated one? "))
            (let ((inhibit-read-only t))
              ;; Only remove the current message (up to comments), keep comments.
              (delete-region (point-min) msg-end)
              (goto-char (point-min))
              (insert (string-trim-right text) "\n")
              (goto-char (point-min)))
            (message "magpt: commit message inserted into %s" (buffer-name buf))
            t))))))

(defun magpt--commit-callback (response info)
  "Callback for gptel. Inserts the response into the commit message buffer.
First tries to use the buffer from :context; if not found or unavailable,
tries to find any active commit message buffer. If there is no buffer, outputs
the result to *Messages*."
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
              (message "magpt/gptel error: %s" errstr))
          (let ((text (string-trim (or response ""))))
            (cond
             ((string-empty-p text)
              (when (buffer-live-p target)
                (magpt--remove-commit-overlay target))
              (message "magpt: empty response from model"))
             (target
              ;; Remove overlay before inserting.
              (magpt--remove-commit-overlay target)
              (if (magpt--insert-into-commit-buffer-target target text)
                  (message "magpt: commit message inserted into commit buffer")
                (message "magpt: insertion cancelled by user")))
             (t
              (when (buffer-live-p commit-buf)
                (magpt--remove-commit-overlay commit-buf))
              (message "magpt: commit buffer unavailable; generated message:\n%s" text))))))
    (error
     (message "magpt: error in callback: %s" (error-message-string err)))))

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
  (unless (and (require 'magit nil t) (fboundp 'magit-commit-create))
    (user-error "magpt-commit-staged requires Magit"))
  (let* ((root (magpt--project-root))
         (diff (magpt--staged-diff root)))
    (if (string-empty-p (string-trim diff))
        (message "magpt: No staged changes found (git add ...)")
      (let* ((trunc (magpt--maybe-truncate diff magpt-max-diff-bytes))
             (diff (car trunc))
             (truncatedp (cdr trunc))
             (prompt (magpt--build-commit-prompt magpt-commit-prompt diff truncatedp))
             (target-buf (and (magpt--commit-buffer-p) (current-buffer))))
        (if (and target-buf (buffer-live-p target-buf))
            ;; Уже в commit-буфере: сразу показываем overlay и запускаем генерацию.
            (progn
              (magpt--show-commit-overlay target-buf)
              (message "magpt: requesting LLM to generate commit message...")
              (condition-case err
                  (let ((gptel-model (or magpt-model gptel-model)))
                    (gptel-request prompt :context target-buf :callback #'magpt--commit-callback))
                (error
                 (magpt--remove-commit-overlay target-buf)
                 (message "magpt: error calling gptel: %s" (error-message-string err)))))
          ;; Иначе: ждём открытия окна/буфера с помощью git-commit-setup-hook и запускаем генерацию после его подготовки.
          (let (hook-fn remove-timer)
            (setq hook-fn
                  (lambda ()
                    ;; Одноразовый хук — сразу снимаем.
                    (when (timerp remove-timer) (cancel-timer remove-timer))
                    (remove-hook 'git-commit-setup-hook hook-fn)
                    (let ((buf (current-buffer)))
                      (when (magpt--commit-buffer-p buf)
                        (magpt--show-commit-overlay buf)
                        (message "magpt: requesting LLM to generate commit message...")
                        (condition-case err
                            (let ((gptel-model (or magpt-model gptel-model)))
                              (gptel-request prompt :context buf :callback #'magpt--commit-callback))
                          (error
                           (magpt--remove-commit-overlay buf)
                           (message "magpt: error calling gptel: %s" (error-message-string err))))))))
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
               (message "magpt: could not open Magit commit buffer: %s" (error-message-string err))))))))))

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

(provide 'magpt)

;;; magpt.el ends here
