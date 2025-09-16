;;; magpt-overview-extras.el --- Extra AI overview cards for MaGPT -*- lexical-binding: t; -*-

;;; Commentary:
;; Show AI advice cards for specialized commands even if explain-status
;; is not available yet. We do not modify existing overview; we add a
;; supplemental section to Magit Status.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'magit nil t)
(require 'magpt-log)
(require 'magpt-history nil t)
(declare-function magpt--i18n "ext:magpt" (key &rest args))

(defgroup magpt-overview-extras nil
  "Extra AI overview cards."
  :group 'magpt)

(defcustom magpt-overview-extra-tasks
  '(explain-undo-commits
    explain-stash
    explain-branches
    explain-push-pull
    reset-files-suggest
    restore-file-suggest
    explain-reflog-rescue
    explain-detached-head
    explain-set-upstream)
  "Tasks to surface as extra cards in AI overview."
  :type '(repeat symbol)
  :group 'magpt-overview-extras)

(defcustom magpt-overview-extras-max-cards 6
  "Maximum number of extra cards to insert."
  :type 'integer
  :group 'magpt-overview-extras)

(defun magpt-overview-extras--json->plist (s)
  (when (and (stringp s) (> (length s) 1))
    (or (ignore-errors
          (json-parse-string s :object-type 'plist :array-type 'list))
        (ignore-errors
          (let ((json-object-type 'plist)
                (json-array-type 'list))
            (json-read-from-string s)))
        nil)))

(defun magpt-overview-extras--last-entry (task)
  "Return newest history entry plist for TASK from `magpt--history-entries'."
  (when (boundp 'magpt--history-entries)
    (cl-find task magpt--history-entries
             :key (lambda (e) (plist-get e :task))
             :test #'eq)))

(defun magpt-overview-extras--task-title (task)
  "Return localized title for TASK using magpt i18n keys."
  (pcase task
    ('explain-undo-commits  (magpt--i18n 'overview-card-undo-commits))
    ('explain-stash         (magpt--i18n 'overview-card-stash))
    ('explain-branches      (magpt--i18n 'overview-card-branches))
    ('explain-push-pull     (magpt--i18n 'overview-card-push-pull))
    ('reset-files-suggest   (magpt--i18n 'overview-card-reset-files))
    ('restore-file-suggest  (magpt--i18n 'overview-card-restore-file))
    ('explain-reflog-rescue (magpt--i18n 'overview-card-reflog-rescue))
    ('explain-detached-head (magpt--i18n 'overview-card-detached-head))
    ('explain-set-upstream  (magpt--i18n 'overview-card-set-upstream))
    (_ (format "%s" task))))

(defun magpt-overview-extras--entry->summary (entry)
  "Return a short summary string for ENTRY, trying several known fields.
Prefer :summary if present, otherwise try to parse a JSON :summary from
:response (most history entries store the raw model output in :response).
Fall back to :raw for older entries, or show an error note when entry is invalid."
  (let* ((valid (plist-get entry :valid))
         (summary
          ;; 1) explicit :summary field
          (or (plist-get entry :summary)
              ;; 2) try to parse :response (preferred new key)
              (let ((resp (plist-get entry :response)))
                (when (and (stringp resp) (> (length (string-trim resp)) 0))
                  (let ((obj (magpt-overview-extras--json->plist resp)))
                    (when obj
                      (let ((s (plist-get obj :summary)))
                        (and (stringp s) s))))))))
         (raw (plist-get entry :raw)))
    (or summary
        ;; 3) legacy :raw field
        (when raw
          (let* ((obj (magpt-overview-extras--json->plist raw))
                 (s (plist-get obj :summary)))
            (and (stringp s) s)))
        ;; 4) if entry marked invalid, show its error
        (and (not valid)
             (let ((e (plist-get entry :error)))
               (and (stringp e) (format "Ошибка: %s" e)))))))

(defun magpt-overview-extras--insert-card (task entry)
  (let* ((title (magpt-overview-extras--task-title task))
         (summary (or (magpt-overview-extras--entry->summary entry) "")))
    (magit-insert-section (magit-section 'magpt-extra-card task)
      (magit-insert-heading (format "  • %s" title))
      (when (> (length summary) 0)
        (dolist (ln (split-string summary "\n"))
          (insert "    " ln "\n")))
      (insert "\n"))))

(defun magpt-overview-extras--have-explain-status? ()
  (let ((e (magpt-overview-extras--last-entry 'explain-status)))
    (and e (plist-get e :valid))))

(defun magpt-overview-extras-insert ()
  "Insert extra AI overview cards into Magit Status buffer."
  (when (derived-mode-p 'magit-status-mode)
    (let ((inserted 0))
      (magit-insert-section (magit-section 'magpt-overview-extras)
        (magit-insert-heading (magpt--i18n 'overview-extras-title))
        ;; Hint about [. g] only if main explain-status is missing.
        (when (not (magpt-overview-extras--have-explain-status?))
          (insert (propertize (concat "  " (magpt--i18n 'overview-extras-hint-dot-g) "\n\n")
                              'face 'magit-dimmed)))
        ;; Make each card collapsible and hidden by default.
        (let ((magit-section-initial-visibility-alist
               (cons (cons 'magpt-extra-card 'hide)
                     magit-section-initial-visibility-alist)))
          (dolist (task magpt-overview-extra-tasks)
            (when (< inserted magpt-overview-extras-max-cards)
              (let ((entry (magpt-overview-extras--last-entry task)))
                (when (and entry (plist-get entry :valid))
                  (magpt-overview-extras--insert-card task entry)
                  (cl-incf inserted))))))
        (when (= inserted 0)
          (insert (propertize (concat "  " (magpt--i18n 'overview-extras-no-data) "\n\n")
                              'face 'magit-dimmed)))))))

;; Hook into Magit status render
(defvar magpt-overview-extras--hook-added nil)

;;;###autoload
(defun magpt-overview-extras-enable ()
  "Enable insertion of extra AI overview cards at the BOTTOM of magit-status."
  (unless magpt-overview-extras--hook-added
    ;; Append to ensure extras appear at the bottom (after builtin sections).
    (add-hook 'magit-status-sections-hook #'magpt-overview-extras-insert t)
    (setq magpt-overview-extras--hook-added t)))

(provide 'magpt-overview-extras)

;;; magpt-overview-extras.el ends here
