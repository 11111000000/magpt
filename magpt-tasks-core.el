;;; magpt-tasks-core.el --- Core task wrappers for MaGPT  -*- lexical-binding: t; -*-

;; Lightweight wrappers to access the existing magpt task machinery
;; without duplicating implementations in magpt.el.

(defvar magpt--tasks)
(declare-function magpt--hash-table-keys "ext:magpt" (ht))
;;;###autoload
(defun magpt-tasks-core-run-task (name &optional ctx)
  "Run a registered task by NAME with optional CTX.
This is a thin wrapper that delegates to the main `magpt-run-task'
defined in magpt.el."
  (interactive
   (list (intern (completing-read "magpt task: "
                                  (mapcar #'symbol-name (magpt--hash-table-keys magpt--tasks))))))
  (if (fboundp 'magpt-run-task)
      (magpt-run-task name ctx)
    (user-error "magpt-run-task is not available")))

(provide 'magpt-tasks-core)

;;; magpt-tasks-core.el ends here
