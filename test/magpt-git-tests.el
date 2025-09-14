;;; magpt-git-tests.el --- Git-related tests -*- lexical-binding: t; -*-

;; To run:
;;   emacs -Q -batch -L . -L test -l test/test-helper.el \
;;     -l test/magpt-git-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'magpt-git)

(ert-deftest magpt-project-root-detection ()
  "magpt--project-root should return the same as rev-parse --show-toplevel for all strategies."
  (magpt-test--skip-unless-git)
  (let ((default-directory magpt-test--project-root))
    (let ((expected (magpt--git-root-from default-directory)))
      (should (stringp expected))
      (dolist (strategy '(prefer-magit prefer-vc prefer-project))
        (let ((magpt-project-root-strategy strategy))
          (should (equal (magpt--project-root) expected)))))))

(ert-deftest magpt-string-bytes-basic ()
  "UTF-8 byte counting should be correct."
  (should (= (magpt--string-bytes "Aж") 3))   ;; 1 + 2
  (should (= (magpt--string-bytes "😺") 4)))

(ert-deftest magpt-process-git-and-recent-output-log ()
  "process-git should return (exit . output) and append to recent output cache."
  (magpt-test--with-temp-git-repo
   (let* ((res (magpt--process-git default-directory "status" "--porcelain"))
          (exit (car res))
          (_out (cdr res))
          (recent (magpt--recent-git-output-get default-directory)))
     (should (integerp exit))
     (should (stringp recent))
     ;; Allow for extra leading -c core.quotepath=false in recent log header.
     (should (string-match-p "status --porcelain" recent)))))

(ert-deftest magpt-git-signal-on-error ()
  "magpt--git should signal user-error on non-zero exit."
  (magpt-test--with-temp-git-repo
   (should-error (magpt--git default-directory "foobar") :type 'user-error)))

(provide 'magpt-git-tests)
;;; magpt-git-tests.el ends here
