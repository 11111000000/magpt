;;; magpt-apply.el --- Apply/entry UI actions for MaGPT  -*- lexical-binding: t; -*-

;; Author: Peter
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, vc, git, ai
;; URL: https://github.com/11111000000/magpt

;;; Commentary:
;; UI actions for entries (copy/open JSON/patch, Eshell insert), helper buttons,
;; basic JSON ensure helper, and apply dispatcher for safe operations.
;; Used by Magit overview and AI actions UI.

;;; Code:

(require 'subr-x)
(require 'json)

;; External deps (forward decls) from other magpt modules.
(declare-function magpt--i18n "ext:magpt" (key &rest args))
(declare-function magpt--project-root "magpt-git")
(declare-function magpt--git "magpt-git" (dir &rest args))
(declare-function magpt--git-apply-temp "magpt-git" (dir patch &rest args))
(declare-function magpt--git-apply-check-temp "magpt-git" (dir patch &rest args))
(declare-function magpt--entry-parse-json-safe "magpt-history" (entry))
(declare-function magpt--commit-buffer-p "magpt-commit" (&optional buf))
(declare-function magpt--find-commit-buffer "magpt-commit" ())
(declare-function magpt--insert-into-commit-buffer-target "magpt-commit" (buf text))
(declare-function magpt--btn-preview-text "magpt-ui-preview" (title text &optional mode))
(defvar magpt--history-entries)
(declare-function magpt--history-last-entry-for "magpt-history" (task))

(defvar magpt-allow-apply-safe-ops)

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

;; Entry helpers and actions

(defun magpt--entry-at-point ()
  "Return the history ENTRY plist at point, or nil if none."
  (let ((pos (point)) entry)
    (setq entry (get-text-property pos 'magpt-entry))
    (while (and (null entry) (> pos (point-min)))
      (setq pos (1- (or (previous-single-property-change pos 'magpt-entry nil (point-min))
                        (point-min))))
      (setq entry (get-text-property pos 'magpt-entry)))
    entry))

(defun magpt-open-response-json (&optional entry)
  "Open ENTRY's response in a JSON buffer and pretty-print when valid."
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
      (message "%s" (magpt--i18n 'json-opened)))))

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
    (message "%s" (magpt--i18n 'patch-opened))))

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
           (ok (zerop (car check))))
      (if (not ok)
          (message "magpt: patch --check failed; not applying")
        (when (y-or-n-p "Apply patch to index (git apply --cached)? ")
          (ignore-errors (magpt--git-apply-temp root resp "--cached"))
          (message "magpt: patch applied to index"))))))

;; Eshell insert

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

;; Restore file helpers

(defun magpt--entry-target-defaults (entry)
  "Extract default :path and :rev from ENTRY plist, if present."
  (list :path (plist-get entry :target-path)
        :rev  (or (plist-get entry :target-rev) "HEAD")))

(defun magpt--read-restore-file-args (entry)
  "Read (ROOT PATH REV) for preview/restore, defaulting from ENTRY when possible."
  (let* ((root (magpt--project-root))
         (defs (and (listp entry) (magpt--entry-target-defaults entry)))
         (def-path (plist-get defs :path))
         (def-rev  (plist-get defs :rev))
         (cands (ignore-errors (magpt--git root "ls-files" "-co" "--exclude-standard")))
         (ls (and cands (split-string cands "\n" t)))
         (path (completing-read "File to preview/restore: " ls nil t nil nil def-path))
         (rev  (read-string "Revision (commit-ish): " (or def-rev "HEAD"))))
    (list root path rev)))

(defun magpt--btn-restore-file-preview (button)
  "Preview diff or blob for selected REV:PATH in a read-only buffer."
  (let* ((e (button-get button 'magpt-entry))
         (vals (magpt--read-restore-file-args e))
         (root (nth 0 vals)) (path (nth 1 vals)) (rev (nth 2 vals))
         (diff (condition-case _ (magpt--git root "diff" "--no-color" rev "--" path)
                 (error ""))))
    (if (and (stringp diff) (> (length diff) 0))
        (magpt--btn-preview-text (format "diff: %s @ %s" path rev) diff 'diff)
      (let ((blob (condition-case _ (magpt--git root "show" (format "%s:%s" rev path))
                    (error ""))))
        (if (> (length blob) 0)
            (magpt--btn-preview-text (format "show: %s:%s" rev path) blob 'text)
          (user-error "No diff or blob available for %s at %s" path rev))))))

(defun magpt--path-has-unstaged-p (root path)
  "Return non-nil when PATH has unstaged changes."
  (condition-case _ (let ((out (magpt--git root "diff" "--name-only" "--" path)))
                      (> (length (string-trim out)) 0))
    (error nil)))

(defun magpt--btn-restore-file-apply (button)
  "Restore file from REV to worktree/index after confirmation.
Offers stashing unstaged changes first (stash -k) if restoring to worktree."
  (unless magpt-allow-apply-safe-ops
    (user-error "Applying operations is disabled (magpt-allow-apply-safe-ops is nil)"))
  (let* ((e (button-get button 'magpt-entry))
         (vals (magpt--read-restore-file-args e))
         (root (nth 0 vals)) (path (nth 1 vals)) (rev (nth 2 vals))
         (where (completing-read "Restore to: " '("worktree" "index" "both") nil t nil nil "worktree")))
    (when (and (member where '("worktree" "both"))
               (magpt--path-has-unstaged-p root path))
      (when (y-or-n-p (format "Unstaged changes in %s. Stash them first (keep index)? " path))
        (ignore-errors (magpt--git root "stash" "push" "-k" "-m"
                                   (format "magpt: backup %s" path) "--" path))))
    (condition-case err
        (progn
          (when (member where '("index" "both"))
            (magpt--git root "restore" "--source" rev "--staged" "--" path))
          (when (member where '("worktree" "both"))
            (magpt--git root "restore" "--source" rev "--" path))
          (message "magpt: restored %s from %s → %s" path rev where))
      (error (user-error "Git error: %s" (error-message-string err))))))

;; Button inserter

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
                           'magpt-entry entry))
      ('restore-file-suggest
       (insert "  ")
       (insert-text-button "[Preview file@rev]"
                           'action #'magpt--btn-restore-file-preview
                           'follow-link t
                           'help-echo "Show diff or blob for selected revision"
                           'magpt-entry entry)
       (when magpt-allow-apply-safe-ops
         (insert "  ")
         (insert-text-button "[Restore...]"
                             'action #'magpt--btn-restore-file-apply
                             'follow-link t
                             'help-echo "git restore --source REV (worktree/index)"
                             'magpt-entry entry))))
    (insert "\n")
    (put-text-property start (point) 'read-only t)))

;; JSON ensure and apply dispatcher

(defun magpt--entry-ensure-json (entry)
  "Parse and return JSON from ENTRY's response as an alist; signal on error."
  (let ((resp (plist-get entry :response)))
    (condition-case _err
        (json-parse-string (or resp "") :object-type 'alist :array-type 'list)
      (error
       (user-error "Response is not valid JSON for task %s" (plist-get entry :task))))))

;;;###autoload
(defun magpt-stage-by-intent-apply-last ()
  "Apply latest 'Stage by Intent' plan from history (file-level only)."
  (interactive)
  (unless magpt-allow-apply-safe-ops
    (user-error "Applying operations is disabled (magpt-allow-apply-safe-ops is nil)"))
  (let ((e (magpt--history-last-entry-for 'stage-by-intent)))
    (unless e (user-error "No 'stage-by-intent' results in history"))
    (magpt--apply-stage-by-intent-entry e)))

;;;###autoload
(defun magpt-apply-last (task)
  "Apply the most recent result for TASK from history (safe operations only)."
  (interactive)
  (unless magpt-allow-apply-safe-ops
    (user-error "Applying operations is disabled (magpt-allow-apply-safe-ops is nil)"))
  (pcase task
    ('stage-by-intent (call-interactively #'magpt-stage-by-intent-apply-last))
    (_ (user-error "No apply handler for task: %s" task))))

(provide 'magpt-apply)

;;; magpt-apply.el ends here
