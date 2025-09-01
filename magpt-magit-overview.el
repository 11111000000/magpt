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

;; Forward declarations from magpt.el (to silence byte-compiler).
(declare-function magpt--history-last-entry-for "magpt-history" (task))
(declare-function magpt--entry-parse-json-safe "magpt-history" (entry))
(declare-function magpt--i18n "ext:magpt" (key &rest args))
(declare-function magpt--project-root "magpt-git")
(declare-function magpt--git "magpt-git" (dir &rest args))
(declare-function magpt--insert-entry-buttons "magpt-apply" (entry))
(declare-function magpt--btn-eshell-insert "magpt-apply" (button))

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
                         (ignore-errors
                           (cond
                            ((fboundp 'magit-refresh) (magit-refresh))
                            ((fboundp 'magit-refresh-buffer) (magit-refresh-buffer)))))))))))

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
            (let* ((raw-resp (plist-get ex :response))
                   (data (or (magpt--entry-parse-json-safe ex)
                             (condition-case _err
                                 (let* ((s (string-trim (or raw-resp ""))))
                                   (when (and (stringp s) (> (length s) 0))
                                     ;; If response is fenced (e.g. =json ... =), strip outer fences and try to parse.
                                     (let* ((lines (split-string s "\n"))
                                            (first (car lines))
                                            (last  (car (last lines)))
                                            (between (if (and first last
                                                              (string-match-p "\\`[`~]\\{3,\\}" first)
                                                              (string-match-p "\\`[`~]\\{3,\\}[ \t]*\\'" last))
                                                         (mapconcat #'identity (butlast (cdr lines)) "\n")
                                                       s)))
                                       (condition-case _ (json-parse-string (string-trim between) :object-type 'alist :array-type 'list)
                                         (error nil)))))
                               (error nil))))
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
                      (insert (format "  … %d more (open JSON)\n" (- (length sugs) (length ss)))))
                    ;; Make sure the action buttons for the entire explain-status
                    ;; entry are *also* inside a magit section node.
                    (when (fboundp 'magpt--insert-entry-buttons)
                      (magit-insert-section (magit-section 'magpt-explain-status-buttons)
                        (magpt--insert-entry-buttons ex))))))
              ;; Fallback only if parsing failed
              (unless (magpt--entry-parse-json-safe ex)
                (magit-insert-section (magit-section 'magpt-explain-status-fallback)
                  (insert (magpt--i18n 'overview-response) "\n")
                  (insert (string-trim-right (plist-get ex :response)) "\n\n")
                  (when (fboundp 'magpt--insert-entry-buttons)
                    (magpt--insert-entry-buttons ex))))))
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
                (when (fboundp 'magpt--insert-entry-buttons)
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
                  (when (fboundp 'magpt--insert-entry-buttons)
                    (magpt--insert-entry-buttons bn))))
              ;; Child section: Resolve Conflict (if present)
              (when rc
                (magit-insert-section (magit-section 'magpt-ai-card-resolve-conflict)
                  (magit-insert-heading "Resolve Conflict (here)")
                  (let ((magpt-ui-density 'compact))
                    (insert (magpt--i18n 'overview-response) "\n")
                    (insert (string-trim-right (plist-get rc :response)) "\n\n")
                    (when (fboundp 'magpt--insert-entry-buttons)
                      (magpt--insert-entry-buttons rc)))))
              ;; Hint line with key shortcut.
              (insert (propertize "  [.] AI actions\n" 'face 'magpt-badge-info-face))
              (insert "\n"))))))))

(provide 'magpt-magit-overview)

;;; magpt-magit-overview.el ends here
