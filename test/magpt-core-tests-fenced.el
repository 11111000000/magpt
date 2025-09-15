;;; magpt-core-tests-fenced.el --- Tests for fenced JSON handling -*- lexical-binding: t; -*-
(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'magpt)

(defvar magpt--test-last-message nil)

(defmacro magpt--capture-message (&rest body)
  `(cl-letf* (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq magpt--test-last-message (apply 'format fmt args))
                 magpt--test-last-message)))
     ,@body))
(put 'magpt--capture-message 'lisp-indent-function 0)

(ert-deftest magpt-run-task-callback-fenced-json-ok-message ()
  "Ensure fenced JSON is recognized as valid (same as history sanitization)."
  (setq magpt--test-last-message nil)
  (let* ((task (magpt--task
                :name 'dummy-fenced
                :title "Dummy"
                :scope 'repo
                :context-fn (lambda (_ctx) (list nil "preview" 7))
                :prompt-fn (lambda (_d) "PROMPT")
                :render-fn (lambda (_o _d) nil)
                :apply-fn nil
                :confirm-send? nil))
         (resp "=json\n{\"summary\": \"ok\"}\n="))
    (cl-letf* (((symbol-function 'magpt--gptel-request)
                (lambda (_prompt &rest args)
                  (let ((cb (plist-get args :callback)))
                    (funcall cb resp nil)))))
      (magpt--capture-message
       (magpt--run-task task))
      (should (string-match-p "JSON OK\\|выполнено.*JSON" (or magpt--test-last-message ""))))))

(provide 'magpt-core-tests-fenced)
