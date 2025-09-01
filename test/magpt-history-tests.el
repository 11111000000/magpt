;;; magpt-history-tests.el --- History tests -*- lexical-binding: t; -*-

;; To run:
;;   emacs -Q -batch -L . -L test -l test/test-helper.el \
;;     -l test/magpt-history-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'magpt-history)

(ert-deftest magpt-history-records-request-preview ()
  "History entry should store request preview and validate JSON."
  (let ((magpt--history-entries nil))
    (magpt--history-append-entry 'explain-status "PROMPT-X" "{\"x\":1}")
    (let ((e (car magpt--history-entries)))
      (should (equal (plist-get e :request) "PROMPT-X"))
      (should (equal (plist-get e :response) "{\"x\":1}"))
      (should (eq (plist-get e :valid) t)))))

(ert-deftest magpt-history-invalid-json ()
  "Invalid JSON should be marked as not valid."
  (let ((magpt--history-entries nil))
    (magpt--history-append-entry 'x "REQ" "{not-json")
    (let ((e (car magpt--history-entries)))
      (should (eq (plist-get e :valid) nil)))))

(ert-deftest magpt-history-max-entries-trim ()
  "History should be trimmed to magpt-history-max-entries."
  (let ((magpt--history-entries nil)
        (magpt-history-max-entries 2))
    (magpt--history-append-entry 'a "" "{}")
    (magpt--history-append-entry 'b "" "{}")
    (magpt--history-append-entry 'c "" "{}")
    (should (= (length magpt--history-entries) 2))
    (should (eq (plist-get (car magpt--history-entries) :task) 'c))
    (should (eq (plist-get (cadr magpt--history-entries) :task) 'b))))

(provide 'magpt-history-tests)
;;; magpt-history-tests.el ends here
