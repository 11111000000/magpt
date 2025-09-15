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
  (pcase task
    ('explain-undo-commits "Отмена коммитов (reset vs revert)")
    ('explain-stash        "Stash: push/apply/pop/branch")
    ('explain-branches     "Ветви: обзор и работа")
    ('explain-push-pull    "Push/Pull: рекомендации")
    ('reset-files-suggest  "Сброс файлов: варианты")
    ('restore-file-suggest "Восстановление файла")
    ('explain-reflog-rescue "Reflog: спасение")
    ('explain-detached-head "Detached HEAD: помощь")
    ('explain-set-upstream  "Upstream: настройка")
    (_ (format "%s" task))))

(defun magpt-overview-extras--entry->summary (entry)
  (let* ((valid (plist-get entry :valid))
         (summary (plist-get entry :summary))
         (raw (plist-get entry :raw)))
    (or summary
        (when raw
          (let* ((obj (magpt-overview-extras--json->plist raw))
                 (s (plist-get obj :summary)))
            (and (stringp s) s)))
        (and (not valid)
             (let ((e (plist-get entry :error)))
               (and (stringp e) (format "Ошибка: %s" e)))))))

(defun magpt-overview-extras--insert-card (task entry)
  (let* ((title (magpt-overview-extras--task-title task))
         (summary (or (magpt-overview-extras--entry->summary entry) "")))
    (insert (propertize (format "  • %s\n" title) 'face 'magit-section-heading))
    (when (> (length summary) 0)
      (let ((short (if (> (length summary) 240)
                       (concat (substring summary 0 237) "…")
                     summary)))
        (insert (format "    %s\n" short))))
    (insert "\n")))

(defun magpt-overview-extras--have-explain-status? ()
  (let ((e (magpt-overview-extras--last-entry 'explain-status)))
    (and e (plist-get e :valid))))

(defun magpt-overview-extras-insert ()
  "Insert extra AI overview cards into Magit Status buffer."
  (when (and (derived-mode-p 'magit-status-mode)
             (boundp 'magpt--history-entries)
             magpt--history-entries)
    (let ((inserted 0))
      (insert (propertize "AI-обзор (дополнительно)\n" 'face 'magit-section-heading))
      ;; Hint about [. g] only if main explain-status is missing.
      (when (not (magpt-overview-extras--have-explain-status?))
        (insert (propertize "  (Рекомендуется запустить [. g] для сводки статуса.)\n\n"
                            'face 'magit-dimmed)))
      (dolist (task magpt-overview-extra-tasks)
        (when (< inserted magpt-overview-extras-max-cards)
          (let ((entry (magpt-overview-extras--last-entry task)))
            (when (and entry (plist-get entry :valid))
              (magpt-overview-extras--insert-card task entry)
              (cl-incf inserted)))))
      (when (= inserted 0)
        (insert (propertize "  (Пока нет данных по дополнительным задачам)\n\n"
                            'face 'magit-dimmed))))))

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
