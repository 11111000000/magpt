;;; magpt-magit-overview-tests.el --- Magit overview tests (safe subset) -*- lexical-binding: t; -*-

;; To run:
;;   emacs -Q -batch -L . -L test -l test/test-helper.el \
;;     -l test/magpt-magit-overview-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'magpt-magit-overview)

(ert-deftest magpt-request-extract-status ()
  "Extract STATUS block between markers."
  (let* ((req "foo\n--- BEGIN STATUS ---\na\nb\n--- END STATUS ---\nbar"))
    (should (equal (magpt--request-extract-status req) "a\nb"))))

(ert-deftest magpt-status-lines-equal-p-ignores-order-and-blanks ()
  "Status line comparison should ignore order and blank lines."
  (let* ((a " M a.txt\n\n?? x\n")
         (b "?? x\n M a.txt\n"))
    (should (magpt--status-lines-equal-p a b))))

(provide 'magpt-magit-overview-tests)
;;; magpt-magit-overview-tests.el ends here
