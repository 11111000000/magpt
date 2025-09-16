;;; magpt-magit-overview.el --- Magit Status AI overview for MaGPT  -*- lexical-binding: t; -*-

;; Author: Peter
;; Package-Requires: ((emacs "28.1") (magit "3.0"))
;; Keywords: tools, vc, git, ai
;; URL: https://github.com/11111000000/magpt

;;; Commentary:
;; Renders a compact AI overview in magit-status and refreshes it on history changes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'json)
(require 'magit nil t)
(require 'magpt-apply nil t)
(require 'magpt-history nil t)
(require 'magpt-log nil t)
(require 'magpt-ui-icons nil t)

(defcustom magpt-magit-overview-enabled t
  "If non-nil, insert a compact 'AI overview (magpt)' section into magit-status."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-magit-auto-refresh t
  "If non-nil, automatically refresh visible Magit status buffers when AI history changes.
When non-nil, magpt will programmatically call `magit-refresh' after tasks complete,
which may affect section visibility in some Magit versions. Set this to nil to avoid
automatic refresh and preserve the user's manual section visibility."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-magit-preserve-visibility t
  "If non-nil, try to preserve Magit section visibility on auto-refresh.
Uses Magit's save/restore helpers when available."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-overview-show-educational-fields nil
  "If non-nil, show optional educational fields (rationale/steps) in suggestions."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-overview-show-actions-hint nil
  "If non-nil, show bottom hint line with key shortcut in AI overview."
  :type 'boolean
  :group 'magpt)

(defface magpt-console-face
  '((t :background "black" :foreground "#e6e6e6" :extend t))
  "Face for console-like command examples in Magpt overview."
  :group 'magpt)

;; Forward declarations from magpt.el (to silence byte-compiler).
(declare-function magpt--history-last-entry-for "magpt-history" (task))
(declare-function magpt--entry-parse-json-safe "magpt-history" (entry))
(declare-function magpt--i18n "ext:magpt" (key &rest args))
(declare-function magpt--project-root "magpt-git")
(declare-function magpt--git "magpt-git" (dir &rest args))
(declare-function magpt--insert-entry-buttons "magpt-apply" (entry))
(declare-function magpt--btn-eshell-insert "magpt-apply" (button))
(declare-function magpt-explain-status "magpt-tasks-assist" ())
(declare-function magpt--icon "ext:magpt" (key))

(defvar magpt-ui-density)
(defvar magpt-overview-compact-max-risks)
(defvar magpt-overview-compact-max-suggestions)
(defvar magpt-history-changed-hook)

;; Helper: refresh visible Magit status buffers to update AI overview on new data.
(defcustom magpt-magit-debug-log-refresh nil
  "If non-nil, log Magit refresh/section operations to `magpt-log-buffer-name' for diagnosis.
When enabled, magpt will advice common Magit refresh/section functions and emit
a short backtrace snippet to help find what triggers automatic expansion of sections
(e.g., Staged/Unstaged diffs)."
  :type 'boolean
  :group 'magpt)

(defun magpt--magit-backtrace-snippet (&optional max-chars)
  "Return a short backtrace string (up to MAX-CHARS, default 800)."
  (let ((max (or max-chars 800)))
    (condition-case _
        (let ((bt (with-output-to-string (backtrace))))
          (if (> (length bt) max)
              (concat (substring bt 0 max) "…")
            bt))
      (error "<no-backtrace>"))))

(defun magpt--log-magit-refresh (&rest _args)
  "Log a Magit refresh invocation (used for debugging)."
  (when (and (boundp 'magpt-magit-debug-log-refresh) magpt-magit-debug-log-refresh)
    (when (fboundp 'magpt--log)
      (magpt--log "magit-refresh called; caller-backtrace:\n%s"
                  (magpt--magit-backtrace-snippet 1200)))))

(defun magpt--log-magit-section-show (&rest args)
  "Log Magit section show/hide calls (ARGS contains section)."
  (when (and (boundp 'magpt-magit-debug-log-refresh) magpt-magit-debug-log-refresh)
    (let* ((sec (car args))
           (typ (and (fboundp 'magit-section-type) (ignore-errors (magit-section-type sec)))))
      (when (fboundp 'magpt--log)
        (magpt--log "magit-section-show/hide called for section type=%S id=%S; bt:\n%s"
                    typ sec
                    (magpt--magit-backtrace-snippet 800))))))

;; Install lightweight advices for diagnosis when requested.
(when (and (boundp 'magpt-magit-debug-log-refresh) magpt-magit-debug-log-refresh
           (fboundp 'advice-add))
  (when (fboundp 'magit-refresh)
    (advice-add 'magit-refresh :before #'magpt--log-magit-refresh))
  (when (fboundp 'magit-refresh-buffer)
    (advice-add 'magit-refresh-buffer :before #'magpt--log-magit-refresh))
  ;; magit-section-show exists in Magit 3.x; if available, log calls that affect visibility.
  (when (fboundp 'magit-section-show)
    (advice-add 'magit-section-show :after #'magpt--log-magit-section-show))
  (when (fboundp 'magit-section-hide)
    (advice-add 'magit-section-hide :after #'magpt--log-magit-section-show))
  ;; Also watch for the higher-level expand-all helpers, if present.
  (when (fboundp 'magit-section-toggle)
    (advice-add 'magit-section-toggle :after #'magpt--log-magit-section-show)))

(defun magpt--refresh-magit-status-visible ()
  "Refresh visible Magit status buffers (if any) to update AI overview.
Respect `magpt-magit-auto-refresh' — when nil do nothing.

Schedule the actual refresh with `run-at-time' to avoid performing refreshes
during redisplay callbacks (which can cause redisplay errors in some setups).
This also ensures the Magit status buffer is updated asynchronously after
history changes (no need for the user to press \"g\")."
  (when (and magpt-magit-auto-refresh (featurep 'magit))
    (run-at-time 0 nil
                 (lambda ()
                   (dolist (win (window-list))
                     (with-current-buffer (window-buffer win)
                       (when (derived-mode-p 'magit-status-mode)
                         (let ((vis (and magpt-magit-preserve-visibility
                                         (fboundp 'magit-save-section-visibility)
                                         (condition-case _ (magit-save-section-visibility) (error nil)))))
                           (ignore-errors
                             (cond
                              ((fboundp 'magit-refresh) (magit-refresh))
                              ((fboundp 'magit-refresh-buffer) (magit-refresh-buffer))))
                           (when (and vis magpt-magit-preserve-visibility
                                      (fboundp 'magit-restore-section-visibility))
                             (ignore-errors (magit-restore-section-visibility vis)))))))))))

;; Subscribe Magit overview refresh to history changes.
(add-hook 'magpt-history-changed-hook #'magpt--refresh-magit-status-visible)

;;; Mini renderer for Explain Status inside Magit: use child sections so only headings highlight.

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
  (let* ((sa (sort (split-string (or a "") "\n" t) #'string<))
         (sb (sort (split-string (or b "") "\n" t) #'string<)))
    (equal sa sb)))

(defun magpt--overview--insert-toggle-line ()
  "Insert the toggle button for educational fields."
  (insert "  ")
  (insert-text-button
   (if magpt-overview-show-educational-fields
       (magpt--i18n 'overview-toggle-hide)
     (magpt--i18n 'overview-toggle-show))
   'action (lambda (_btn)
             (setq magpt-overview-show-educational-fields (not magpt-overview-show-educational-fields))
             (when (fboundp 'magit-refresh) (ignore-errors (magit-refresh))))
   'follow-link t
   'help-echo "Toggle showing rationale/steps in suggestions")
  (insert "\n"))

(defun magpt--overview--stale-p (ex)
  "Return non-nil if Explain Status entry EX is stale vs current porcelain."
  (condition-case _
      (let* ((snapshot (plist-get ex :status-snapshot))
             (req (plist-get ex :request))
             (old (or snapshot
                      (and (stringp req) (magpt--request-extract-status req))))
             (root (magpt--project-root))
             (porc (magpt--git root "status" "--porcelain"))
             (cur (string-join (seq-take (split-string porc "\n" t) 200) "\n")))
        (and old (not (magpt--status-lines-equal-p old cur))))
    (error nil)))

(defun magpt--overview--entry-data (entry)
  "Return parsed JSON alist for ENTRY, with markdown-fence fallback."
  (or (magpt--entry-parse-json-safe entry)
      (condition-case _err
          (let* ((s (string-trim (or (plist-get entry :response) ""))))
            (when (and (stringp s) (> (length s) 0))
              (let* ((lines (split-string s "\n"))
                     (first (car lines))
                     (last  (car (last lines)))
                     (between (if (and first last
                                       (string-match-p "\\=[=~]\\{3,\\}" first)
                                       (string-match-p "\\=[=~]\\{3,\\}[ \t]*\\'" last))
                                  (mapconcat #'identity (butlast (cdr lines)) "\n")
                                s)))
                (condition-case _ (json-parse-string (string-trim between)
                                                     :object-type 'alist :array-type 'list)
                  (error nil)))))
        (error nil))))

(defun magpt--overview--insert-summary (summary)
  "Insert Summary subsection with SUMMARY string."
  (when (stringp summary)
    (magit-insert-section (magit-section 'magpt-ai-explain-summary)
      (let ((ic (and (fboundp 'magpt--icon) (magpt--icon 'summary))))
        (magit-insert-heading (concat (if (and ic (> (length ic) 0)) (concat ic " ") "") (magpt--i18n 'overview-summary))))
      (dolist (ln (split-string (string-trim-right summary) "\n"))
        (insert "  " ln "\n")))))

(defun magpt--overview--insert-risks (risks)
  "Insert Risks subsection for RISKS list, honoring compact settings."
  (when (listp risks)
    (let* ((compact (eq magpt-ui-density 'compact))
           (max-r (and compact magpt-overview-compact-max-risks))
           (rs (if max-r (seq-take risks max-r) risks)))
      (magit-insert-section (magit-section 'magpt-ai-explain-risks)
        (let ((ic (and (fboundp 'magpt--icon) (magpt--icon 'risk))))
          (magit-insert-heading (concat (if (and ic (> (length ic) 0)) (concat ic " ") "") (magpt--i18n 'overview-risks))))
        (if rs
            (dolist (r rs) (insert "  • " (format "%s" r) "\n"))
          (insert "  • none\n"))
        (when (and compact (> (length risks) (length rs)))
          (insert (format "  … %d more\n" (- (length risks) (length rs)))))))))

(defun magpt--overview--insert-suggestions (sugs)
  "Insert Suggestions subsection for SUGS list, honoring compact settings."
  (when (listp sugs)
    (let* ((compact (eq magpt-ui-density 'compact))
           (max-s (and compact magpt-overview-compact-max-suggestions))
           (ss (if max-s (seq-take sugs max-s) sugs))
           (i 1))
      (magit-insert-section (magit-section 'magpt-ai-explain-suggestions)
        (magit-insert-heading (concat "🟢 " (magpt--i18n 'overview-suggestions)))
        (let ((magit-section-initial-visibility-alist
               (cons (cons 'magpt-ai-suggestion 'show)
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
              (magit-insert-section (magit-section 'magpt-ai-suggestion (list :index i :data s))
                (magit-insert-heading
                  (format "  %d) 🟢 %s%s"
                          i
                          title
                          (if keys-str (format " [%s]" keys-str) "")))
                (when (and (stringp first-cmd) (> (length first-cmd) 0))
                  (let ((cmdline (format "      $ %s" first-cmd)))
                    (insert (propertize cmdline 'face 'magpt-console-face))
                    (insert "\n"))
                  (insert "      ")
                  (let ((label (let ((ic (and (fboundp 'magpt--icon) (magpt--icon 'eshell-button))))
                                 (if (and ic (> (length ic) 0)) (format "[%s Eshell]" ic) "[Eshell]"))))
                    (insert-text-button label
                                        'action #'magpt--btn-eshell-insert
                                        'follow-link t
                                        'help-echo "Insert command into eshell (bottom popup)"
                                        'magpt-command first-cmd)
                    (insert "\n"))
                  (when magpt-overview-show-educational-fields
                    (let ((rat (alist-get 'rationale s))
                          (steps (alist-get 'steps s)))
                      (when (stringp rat)
                        (insert (format "      %s\n" (magpt--i18n 'overview-rationale)))
                        (dolist (ln (split-string (string-trim-right rat) "\n"))
                          (insert "        " ln "\n")))
                      (when (listp steps)
                        (insert (format "      %s\n" (magpt--i18n 'overview-steps)))
                        (dolist (st steps)
                          (insert "        - " (format "%s" st) "\n")))))))
              (setq i (1+ i)))))
        (when (and compact (> (length sugs) (length ss)))
          (insert (format "  … %d more (open JSON)\n" (- (length sugs) (length ss)))))))))

(defun magpt--overview--insert-explain-status (ex)
  "Insert the Explain Status block for entry EX."
  (when (magpt--overview--stale-p ex)
    (insert (format "  %s\n" (magpt--i18n 'overview-stale))))
  (let* ((data (magpt--overview--entry-data ex))
         (summary (and data (alist-get 'summary data)))
         (risks (and data (alist-get 'risks data)))
         (sugs  (and data (alist-get 'suggestions data))))
    (magpt--overview--insert-summary summary)
    (magpt--overview--insert-risks risks)
    (magpt--overview--insert-suggestions sugs)
    (when (fboundp 'magpt--insert-entry-buttons)
      (magit-insert-section (magit-section 'magpt-explain-status-buttons)
        (magpt--insert-entry-buttons ex))))
  ;; Fallback only when no JSON could be parsed at all.
  (unless (magpt--overview--entry-data ex)
    (magit-insert-section (magit-section 'magpt-explain-status-fallback)
      (insert (magpt--i18n 'overview-response) "\n")
      (insert (string-trim-right (plist-get ex :response)) "\n\n")
      (when (fboundp 'magpt--insert-entry-buttons)
        (magpt--insert-entry-buttons ex)))))

(defun magpt--overview--insert-commit-lint-card (cl)
  "Insert Commit Lint/Fix card for entry CL."
  (when cl
    (magit-insert-section (magit-section 'magpt-ai-card-commit-lint)
      (let ((ic (and (fboundp 'magpt--icon) (magpt--icon 'commit-lint))))
        (magit-insert-heading (concat (if (and ic (> (length ic) 0)) (concat ic " ") "") (magpt--i18n 'overview-card-commit-lint))))
      (let ((magpt-ui-density 'compact))
        (let ((data (magpt--overview--entry-data cl)))
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
        (when (fboundp 'magpt--insert-entry-buttons)
          (magpt--insert-entry-buttons cl))))))

(defun magpt--overview--insert-branch-name-card (bn)
  "Insert Branch Name Suggest card for entry BN."
  (when bn
    (magit-insert-section (magit-section 'magpt-ai-card-branch-name)
      (let ((ic (and (fboundp 'magpt--icon) (magpt--icon 'branch-name))))
        (magit-insert-heading (concat (if (and ic (> (length ic) 0)) (concat ic " ") "") (magpt--i18n 'overview-card-branch-name))))
      (let ((magpt-ui-density 'compact))
        (let ((data (magpt--overview--entry-data bn)))
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
        (when (fboundp 'magpt--insert-entry-buttons)
          (magpt--insert-entry-buttons bn))))))

(defun magpt--overview--insert-resolve-conflict (rc)
  "Insert Resolve Conflict card for entry RC (raw patch preview)."
  (when rc
    (magit-insert-section (magit-section 'magpt-ai-card-resolve-conflict)
      (let ((ic (and (fboundp 'magpt--icon) (magpt--icon 'resolve-conflict))))
        (magit-insert-heading (concat (if (and ic (> (length ic) 0)) (concat ic " ") "") (magpt--i18n 'overview-card-resolve-conflict))))
      (let ((magpt-ui-density 'compact))
        (insert (magpt--i18n 'overview-response) "\n")
        (insert (string-trim-right (plist-get rc :response)) "\n\n")
        (when (fboundp 'magpt--insert-entry-buttons)
          (magpt--insert-entry-buttons rc))))))

(defun magpt--overview--insert-simple-card (section-symbol heading-key entry)
  "Insert a generic advice card for ENTRY with SECTION-SYMBOL and HEADING-KEY.
Card shows summary and first command; falls back to raw response."
  (when entry
    (magit-insert-section (magit-section section-symbol)
      (let* ((ik (pcase heading-key
                   ('overview-card-push-pull     'push-pull)
                   ('overview-card-branches      'branches-overview)
                   ('overview-card-reset-files   'reset-files)
                   ('overview-card-restore-file  'restore-file)
                   ('overview-card-stash         'stash)
                   ('overview-card-undo-commits  'undo-commits)
                   ('overview-card-reflog-rescue 'reflog-rescue)
                   ('overview-card-detached-head 'detached-head)
                   ('overview-card-set-upstream  'set-upstream)
                   (_ nil)))
             (ic (and ik (fboundp 'magpt--icon) (magpt--icon ik))))
        (magit-insert-heading (concat (if (and ic (> (length ic) 0)) (concat ic " ") "") (magpt--i18n heading-key))))
      (let ((magpt-ui-density 'compact))
        (let ((data (magpt--overview--entry-data entry)))
          (when data
            (let* ((summary (alist-get 'summary data))
                   (risks (or (alist-get 'risks data) '()))
                   (sugs (or (alist-get 'suggestions data) '()))
                   (compact (eq magpt-ui-density 'compact))
                   (max-r (and compact magpt-overview-compact-max-risks))
                   (rs (if max-r (seq-take risks max-r) risks))
                   (max-s (and compact magpt-overview-compact-max-suggestions))
                   (ss (if max-s (seq-take sugs max-s) sugs))
                   (i 1))
              ;; Summary
              (when (stringp summary)
                (dolist (ln (split-string (string-trim-right summary) "\n"))
                  (insert "  " ln "\n")))
              ;; Risks (compact-truncated)
              (when (and (listp risks) (> (length risks) 0))
                (insert (format "  %s\n" (magpt--i18n 'overview-risks)))
                (dolist (r rs)
                  (insert "  • " (format "%s" r) "\n"))
                (when (and compact (> (length risks) (length rs)))
                  (insert (format "  … %d more\n" (- (length risks) (length rs)))))
                (insert "\n"))
              ;; Suggestions (multiple, compact-truncated)
              (dolist (s ss)
                (let* ((title (or (alist-get 'title s) (format "Suggestion %d" i)))
                       (keys  (or (alist-get 'keys s) (alist-get 'magit_keys s)))
                       (keys-str (and (listp keys)
                                      (string-join (mapcar (lambda (k) (format "%s" k)) keys) ", ")))
                       (cmds (let ((c (alist-get 'commands s)))
                               (and (listp c) (seq-filter #'stringp c))))
                       (git-cmd (and cmds (seq-find (lambda (c) (string-prefix-p "git " c)) cmds)))
                       (first-cmd (or git-cmd (car cmds))))
                  ;; Title + keys
                  (insert (format "  %d) 🟢 %s%s\n" i title (if keys-str (format " [%s]" keys-str) "")))
                  ;; First command with Eshell button
                  (when (stringp first-cmd)
                    (let ((cmdline (format "      $ %s" first-cmd)))
                      (insert (propertize cmdline 'face 'magpt-console-face))
                      (insert "\n"))
                    (insert "      ")
                    (let ((label (let ((ic (and (fboundp 'magpt--icon) (magpt--icon 'eshell-button))))
                                   (if (and ic (> (length ic) 0)) (format "[%s Eshell]" ic) "[Eshell]"))))
                      (insert-text-button label
                                          'action #'magpt--btn-eshell-insert
                                          'follow-link t
                                          'help-echo "Insert command into eshell (bottom popup)"
                                          'magpt-command first-cmd)
                      (insert "\n")))
                  ;; Optional educational fields
                  (when magpt-overview-show-educational-fields
                    (let ((rat (alist-get 'rationale s))
                          (steps (alist-get 'steps s)))
                      (when (stringp rat)
                        (insert (format "      %s\n" (magpt--i18n 'overview-rationale)))
                        (dolist (ln (split-string (string-trim-right rat) "\n"))
                          (insert "        " ln "\n")))
                      (when (listp steps)
                        (insert (format "      %s\n" (magpt--i18n 'overview-steps)))
                        (dolist (st steps)
                          (insert "        - " (format "%s" st) "\n")))))
                  (insert "\n")
                  (setq i (1+ i))))
              (when (and max-s (> (length sugs) (length ss)))
                (insert (format "  … %d more (open JSON)\n" (- (length sugs) (length ss)))))))
          (when (fboundp 'magpt--insert-entry-buttons)
            (magpt--insert-entry-buttons entry)))))))

(defun magpt--ai-overview--insert-heading-only ()
  "Insert overview heading; log current history size."
  ;; Ensure a blank line above the overview heading for visual separation.
  (insert "\n")
  (let* ((ic (and (fboundp 'magpt--icon)
                  (let ((x (magpt--icon 'ai-overview)))
                    (if (and (stringp x) (> (length x) 0))
                        x
                      (magpt--icon 'suggestion-default))))))
    (magit-insert-heading
      (concat (if (and ic (> (length ic) 0)) (concat ic " ") "")
              (magpt--i18n 'overview-title))))
  (when (fboundp 'magpt--log)
    (magpt--log "overview: insert begin; entries=%d"
                (length (or (and (boundp 'magpt--history-entries) magpt--history-entries) '())))))

(defun magpt--ai-overview--collect-entries ()
  "Collect latest entries for all cards used by the overview."
  (list :ex (magpt--history-last-entry-for 'explain-status)
        :cl (magpt--history-last-entry-for 'commit-lint-suggest)
        :bn (magpt--history-last-entry-for 'branch-name-suggest)
        :rc (magpt--history-last-entry-for 'resolve-conflict-here)
        :pp (magpt--history-last-entry-for 'explain-push-pull)
        :br (magpt--history-last-entry-for 'explain-branches)
        :rs (magpt--history-last-entry-for 'reset-files-suggest)
        :rf (magpt--history-last-entry-for 'restore-file-suggest)
        :st (magpt--history-last-entry-for 'explain-stash)
        :uc (magpt--history-last-entry-for 'explain-undo-commits)
        :rl (magpt--history-last-entry-for 'explain-reflog-rescue)
        :dh (magpt--history-last-entry-for 'explain-detached-head)
        :su (magpt--history-last-entry-for 'explain-set-upstream)))

(defun magpt--ai-overview--insert-no-data-cta ()
  "Insert no-data message and a button to run [. g]."
  (insert (format "  %s\n" (magpt--i18n 'overview-no-data)))
  (insert "  ")
  (insert-text-button (magpt--i18n 'overview-run-dot-g)
                      'action #'magpt-explain-status
                      'follow-link t
                      'help-echo "Collect Explain Status now")
  (insert "\n"))

(defun magpt--ai-overview--insert-cards (entries)
  "Insert all child cards using ENTRIES plist."
  (let ((cl (plist-get entries :cl))
        (bn (plist-get entries :bn))
        (rc (plist-get entries :rc))
        (pp (plist-get entries :pp))
        (br (plist-get entries :br))
        (rs (plist-get entries :rs))
        (rf (plist-get entries :rf))
        (st (plist-get entries :st))
        (uc (plist-get entries :uc))
        (rl (plist-get entries :rl))
        (dh (plist-get entries :dh))
        (su (plist-get entries :su)))
    (magpt--overview--insert-commit-lint-card cl)
    (magpt--overview--insert-branch-name-card bn)
    (magpt--overview--insert-resolve-conflict rc)
    (magpt--overview--insert-simple-card 'magpt-ai-card-push-pull    'overview-card-push-pull    pp)
    (magpt--overview--insert-simple-card 'magpt-ai-card-branches     'overview-card-branches     br)
    (magpt--overview--insert-simple-card 'magpt-ai-card-reset-files  'overview-card-reset-files  rs)
    (magpt--overview--insert-simple-card 'magpt-ai-card-restore-file 'overview-card-restore-file rf)
    (magpt--overview--insert-simple-card 'magpt-ai-card-stash        'overview-card-stash        st)
    (magpt--overview--insert-simple-card 'magpt-ai-card-undo-commits 'overview-card-undo-commits uc)
    (magpt--overview--insert-simple-card 'magpt-ai-card-reflog-rescue 'overview-card-reflog-rescue rl)
    (magpt--overview--insert-simple-card 'magpt-ai-card-detached-head 'overview-card-detached-head dh)
    (magpt--overview--insert-simple-card 'magpt-ai-card-set-upstream  'overview-card-set-upstream  su)))

(defun magpt--ai-overview--insert-actions-hint ()
  "Insert bottom hint line with key shortcut."
  (insert (propertize (concat "  " (magpt--i18n 'ai-actions-hint) "\n")
                      'face 'magpt-badge-info-face))
  (insert "\n"))

(defun magpt-magit-insert-ai-overview ()
  "Insert a compact 'AI overview (magpt)' section into magit-status."
  (when (and magpt-magit-overview-enabled
             (featurep 'magit)
             (fboundp 'magit-insert-section)
             (fboundp 'magit-insert-heading))
    (magit-insert-section (magit-section 'magpt-ai-overview)
      (magpt--ai-overview--insert-heading-only)
      (let* ((entries (magpt--ai-overview--collect-entries))
             (ex (plist-get entries :ex)))
        ;; Insert toggle only when there is something to expand (suggestions or any card).
        (let* ((has-cards (seq-some (lambda (k) (plist-get entries k))
                                    '(:cl :bn :rc :pp :br :rs :rf :st :uc :rl :dh :su)))
               (has-sugs (let* ((d (and ex (magpt--overview--entry-data ex)))
                                (s (and d (alist-get 'suggestions d))))
                           (and (listp s) (> (length s) 0)))))
          (when (or has-cards has-sugs)
            (magpt--overview--insert-toggle-line)))
        (if (not ex)
            (magpt--ai-overview--insert-no-data-cta)
          (magpt--overview--insert-explain-status ex))
        (magpt--ai-overview--insert-cards entries)
        (when magpt-overview-show-actions-hint
          (magpt--ai-overview--insert-actions-hint))))))

(provide 'magpt-magit-overview)

;;; magpt-magit-overview.el ends here
