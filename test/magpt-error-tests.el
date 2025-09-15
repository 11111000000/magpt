;;; magpt-error-tests.el --- Tests for error handling in MaGPT  -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'magpt)

(ert-deftest magpt-log-fallback-no-void-err ()
  "magpt--log should not signal when format args mismatch; it should log a fallback line."
  (let ((magpt-log-enabled t))
    (should
     (condition-case _ex
         (progn
           ;; Intentionally pass fewer args than format requires to trigger fallback.
           (magpt--log "this will %s and %s (missing-arg %s)" "log" "fallback")
           t)
       (error nil)))
    (with-current-buffer (get-buffer-create magpt-log-buffer-name)
      (goto-char (point-min))
      (should (re-search-forward "LOG-FMT-ERROR:" nil t)))))

(ert-deftest magpt-run-task-catches-errors ()
  "magpt--run-task should catch errors and not leak void-variable err; it should log and message."
  (let ((magpt-log-enabled t)
        (magpt-enable-task-registry t))
    ;; Register a failing task
    (magpt-register-task
     (magpt--task
      :name 'magpt--test-failing-task
      :title "Failing test task"
      :scope 'repo
      :context-fn (lambda (_ctx) (error "test boom"))
      :prompt-fn  (lambda (_data) (error "should not be called"))
      :render-fn  (lambda (_out _data) (error "should not render"))
      :apply-fn   nil
      :confirm-send? nil))
    ;; Ensure no error is signaled to the caller
    (should
     (condition-case _ex
         (progn (magpt-run-task 'magpt--test-failing-task)
                t)
       (error nil)))
    ;; Verify log mentions run-task exception
    (with-current-buffer (get-buffer-create magpt-log-buffer-name)
      (goto-char (point-min))
      (should (re-search-forward "run-task exception:" nil t)))))

(provide 'magpt-error-tests)
;;; magpt-error-tests.el ends here
