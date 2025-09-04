;;; magpt-transient.el --- Transient/Magit integration for MaGPT  -*- lexical-binding: t; -*-

;; Author: Peter
;; Package-Requires: ((emacs "28.1") (magit "3.0") (transient "0.3"))
;; Keywords: tools, vc, git, ai
;; URL: https://github.com/11111000000/magpt

;;; Commentary:
;; Isolates Transient/Magit integration: safe transient helpers and `magpt-mode'
;; that injects entries into Magit transient menus and Magit status.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit nil t)
(require 'transient nil t)

;; Forward declarations (silence byte-compiler; implementations in other modules).
(declare-function magpt-commit-staged "ext:magpt")
(declare-function magpt-ai-actions-entry "ext:magpt")
(declare-function magpt-magit-insert-ai-overview "ext:magpt-magit-overview")

(defface magpt-transient-face
  '((t :inherit font-lock-keyword-face :foreground "green3" :weight bold))
  "Face for MaGPT entries in Magit transient menus."
  :group 'magpt)

(defun magpt--transient-desc (s)
  "Return S; kept for future styling hooks."
  s)

;; Safe helpers to integrate with Transient across versions (avoid hard failures).
(defun magpt--transient-append-suffix-safe (parent pos spec)
  "Try to append SPEC after POS in PARENT transient. Return non-nil on success."
  (when (featurep 'transient)
    (condition-case err
        (prog1 t (transient-append-suffix parent pos spec))
      (error
       (when (fboundp 'magpt--log)
         (magpt--log "transient append failed: parent=%S pos=%S err=%s"
                     parent pos (error-message-string err)))
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
        (try `("." ,(magpt--transient-desc "AI actions (magpt)") magpt-ai-actions-entry))))))

;;;###autoload
(define-minor-mode magpt-mode
  "Global minor mode: integrate MaGPT with Magit’s commit transient."
  :global t
  :group 'magpt
  (if magpt-mode
      (with-eval-after-load 'magit
        ;; Commit transient: add AI commit entry
        (magpt--transient-append-suffix-safe 'magit-commit "c"
                                             `("i" ,(magpt--transient-desc "Commit with AI message (magpt)") magpt-commit-staged))
        ;; Magit dispatch: robust insertion (no hard dependency on a specific anchor).
        (magpt--transient-add-to-magit-dispatch)
        ;; Direct key in Magit Status buffer: "." opens AI actions immediately (without dispatch).
        (when (boundp 'magit-status-mode-map)
          (define-key magit-status-mode-map (kbd ".") #'magpt-ai-actions-entry))
        ;; Magit Status: AI overview section (read-only; no background calls)
        ;; Add after built-in sections: append so the AI overview appears at the bottom.
        (add-hook 'magit-status-sections-hook #'magpt-magit-insert-ai-overview t))
    (with-eval-after-load 'magit
      (magpt--transient-remove-suffix-safe 'magit-commit "i")
      (magpt--transient-remove-suffix-safe 'magit-dispatch ".")
      ;; Unbind our direct key when disabling mode.
      (when (boundp 'magit-status-mode-map)
        (define-key magit-status-mode-map (kbd ".") nil))
      (remove-hook 'magit-status-sections-hook #'magpt-magit-insert-ai-overview))))



;; AI Actions transient (moved from magpt.el)

(require 'magpt-ui-preview nil t)
(declare-function magpt--history-last-entry-for "magpt-history" (task))
(declare-function magpt--entry-parse-json-safe "magpt-history" (entry))

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
    (when (fboundp 'magpt--log)
      (magpt--log "ai-actions-init: suggestions=%d summary?=%s"
                  n (if (and (stringp magpt--ai-actions-summary)
                             (> (length magpt--ai-actions-summary) 0))
                        "t" "nil")))
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
      (when (fboundp 'magpt--log)
        (magpt--log "ai-actions-preview: idx=%s title=%s keys=%s"
                    i title (or keys-str "[]")))
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
  (when (featurep 'transient)
    (transient-setup 'magpt-ai-actions))
  (message "magpt: AI actions reloaded from overview"))

(when (featurep 'transient)
  (transient-define-prefix magpt-ai-actions ()
    "AI actions (from last Explain Status result)"
    [["Suggestions"
      ("p" "Preview suggestion..." magpt-ai-actions-preview)
      ("y" "Copy suggestion..." magpt-ai-actions-copy)
      ("s" "Copy summary" magpt-ai-actions-copy-summary)
      ("c" "Commit with AI message" magpt-commit-staged)]
     ["Overview/Tasks"
      ("g" "Get new recommendations (Explain Status)" magpt-explain-status)
      ("r" "Reload from overview" magpt-ai-actions-reload)]]))

(unless (fboundp 'magpt-ai-actions)
  (defun magpt-ai-actions ()
    "Fallback AI actions when `transient' is not available."
    (interactive)
    (magpt--ai-actions-init)
    (call-interactively #'magpt-ai-actions-preview)))

(unless (fboundp 'magpt-ai-actions-entry)
  (defun magpt-ai-actions-entry ()
    "Entry point for '.' key in Magit; logs and opens AI actions."
    (interactive)
    (when (fboundp 'magpt--log)
      (magpt--log "key [.]: magpt-ai-actions-entry buffer=%s root=%s"
                  (buffer-name)
                  (ignore-errors (magpt--project-root))))
    (if (featurep 'transient)
        (transient-setup 'magpt-ai-actions)
      (magpt-ai-actions))))

(provide 'magpt-transient)

;;; magpt-transient.el ends here
