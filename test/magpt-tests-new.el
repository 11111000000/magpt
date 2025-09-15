;;; magpt-tests.el --- ERT tests for MaGPT error handling -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Load core modules needed by tests
(require 'magpt nil t)
(require 'magpt-history nil t)
(require 'magpt-tasks-assist nil t)
(require 'magpt-gpt nil t)
(require 'magpt-git nil t)

(defun magpt-tests--fake-gptel-request (prompt &rest plist)
  "Fake gptel request that schedules a successful callback."
  (let ((cb (plist-get plist :callback)))
    (when cb
      (run-at-time 0 nil
                   (lambda ()
                     (funcall cb
                              "{\"summary\":\"ok\",\"risks\":[],\"suggestions\":[]}"
                              nil)))))
  ;; Return a symbol to emulate a dispatcher object
  'magpt-tests-dispatched)

(defun magpt-tests--fake-gptel-request-void-e (&rest _)
  "Fake gptel request that throws the same error text we observe in the wild."
  (error "Symbol’s value as variable is void: e"))

(defun magpt-tests--fake-git (_dir &rest _args)
  "Return empty output to keep previews small and deterministic."
  "")

(ert-deftest magpt-explain-stash-happy-path ()
  "Ensure explain-stash appends a valid JSON entry when gptel returns normally."
  (cl-letf (((symbol-function 'magpt--gptel-request)
             #'magpt-tests--fake-gptel-request)
            ((symbol-function 'magpt--git) #'magpt-tests--fake-git))
    (setq magpt--history-entries nil)
    (let ((magpt-enable-task-registry t))
      (magpt-explain-stash)
      ;; wait for the callback scheduled with run-at-time 0
      (sleep-for 0.05)
      (should magpt--history-entries)
      (let ((e (car magpt--history-entries)))
        (should (eq (plist-get e :task) 'explain-stash))
        (should (plist-get e :valid))))))

(ert-deftest magpt-explain-stash-void-e-safe ()
  "Ensure explain-stash does not blow up when underlying code signals 'void: e'."
  (cl-letf (((symbol-function 'magpt--gptel-request)
             #'magpt-tests--fake-gptel-request-void-e)
            ((symbol-function 'magpt--git) #'magpt-tests--fake-git))
    (let ((magpt-enable-task-registry t))
      (should (condition-case _ (progn (magpt-explain-stash) t)
                (error nil))))))

(ert-deftest magpt-run-task-sync-error-safe ()
  "If a task's prompt-fn errors synchronously, magpt--run-task must not crash."
  (let* ((magpt-enable-task-registry t)
         (called nil)
         (tsk (magpt--task
               :name 'test-sync-error
               :title "t"
               :scope 'repo
               :context-fn (lambda (_ctx) (list nil "" 1))
               :prompt-fn  (lambda (_d) (error "boom"))
               :render-fn  (lambda (_out _data) (setq called t))
               :apply-fn   nil
               :confirm-send? nil)))
    (magpt-register-task tsk)
    (should (condition-case _ (progn (magpt-run-task 'test-sync-error) t)
              (error nil)))
    (should (null called))))

(provide 'magpt-tests)
;;; magpt-tests.el ends here
