;;; magpt-apply-tests.el --- Apply/UI actions tests -*- lexical-binding: t; -*-

;; To run:
;;   emacs -Q -batch -L . -L test -l test/test-helper.el \
;;     -l test/magpt-apply-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'magpt-apply)

(ert-deftest magpt-apply-stage-intent-executes-ops ()
  "Applying a simple stage-by-intent plan should call git add/restore with expected args."
  (let ((magpt--history-entries nil)
        (magpt-allow-apply-safe-ops t)
        (magpt-test--git-calls nil))
    (push (list :time "T"
                :task 'stage-by-intent
                :request ""
                :response
                "{\"groups\":[{\"title\":\"g1\",\"rationale\":\"x\",\"files\":[{\"path\":\"a.txt\",\"action\":\"stage\"},{\"path\":\"b.txt\",\"action\":\"unstage\"}]}]}")
          magpt--history-entries)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'magpt--project-root) (lambda () default-directory))
              ((symbol-function 'magpt--git)
               (lambda (_dir &rest args)
                 (push args magpt-test--git-calls)
                 "")))
      (magpt-stage-by-intent-apply-last)
      (let ((calls (nreverse magpt-test--git-calls)))
        (should (equal calls
                       '(("add" "--" "a.txt")
                         ("restore" "--staged" "--" "b.txt"))))))))

(ert-deftest magpt-apply-stage-intent-gated-by-allow-flag ()
  "Apply should be gated by magpt-allow-apply-safe-ops."
  (let ((magpt-allow-apply-safe-ops nil))
    (should-error (magpt-stage-by-intent-apply-last) :type 'user-error)))

(ert-deftest magpt-apply-stage-intent-user-declines ()
  "When user declines confirmation, no git calls are made."
  (let ((magpt--history-entries nil)
        (magpt-allow-apply-safe-ops t)
        (magpt-test--git-calls nil))
    (push (list :time "T"
                :task 'stage-by-intent
                :request ""
                :response "{\"groups\":[{\"title\":\"g1\",\"rationale\":\"x\",\"files\":[{\"path\":\"a.txt\",\"action\":\"stage\"}]}]}")
          magpt--history-entries)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function 'magpt--project-root) (lambda () default-directory))
              ((symbol-function 'magpt--git)
               (lambda (_dir &rest args)
                 (push args magpt-test--git-calls)
                 "")))
      (should (equal (magpt-stage-by-intent-apply-last) nil))
      (should (equal magpt-test--git-calls nil)))))

(provide 'magpt-apply-tests)
;;; magpt-apply-tests.el ends here
