;;; magpt-tasks-resolve.el --- Resolve (Phase 3) tasks for MaGPT  -*- lexical-binding: t; -*-

;; Author: Peter
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, vc, git, ai
;; URL: https://github.com/11111000000/magpt

;;; Commentary:
;; Conflict-resolution preview task (explain + propose minimal patch). No apply by default.

;;; Code:

(require 'subr-x)

;; Forward declarations to magpt core
(declare-function magpt--maybe-load-rc "ext:magpt")
(declare-function magpt--string-bytes "magpt-git" (s))
(declare-function magpt--history-append-entry "magpt-history" (task request response &optional note &rest kvs))
(declare-function magpt-register-task "ext:magpt" (task))
(declare-function magpt--task "ext:magpt" (&rest args))
(declare-function magpt-run-task "ext:magpt" (name &optional ctx))

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
  (magpt--history-append-entry 'resolve-conflict-here (or magpt--current-request "") (or patch "")
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

(provide 'magpt-tasks-resolve)

;;; magpt-tasks-resolve.el ends here
