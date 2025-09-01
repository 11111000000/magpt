;;; magpt-gpt-tests.el --- GPT/gptel wrapper tests -*- lexical-binding: t; -*-

;; To run:
;;   emacs -Q -batch -L . -L test -l test/test-helper.el \
;;     -l test/magpt-gpt-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'magpt-gpt)

(ert-deftest magpt-response-to-string-cases ()
  "response->string should tolerate different shapes."
  (should (equal (magpt--response->string "x") "x"))
  (should (equal (magpt--response->string '((content . "ok"))) "ok"))
  (let ((ht (let ((h (make-hash-table :test 'equal)))
              (puthash "a" 1 h) h)))
    (should (string-match-p "{\"a\":1}" (magpt--response->string ht))))
  (should (string-match-p "\\[1,2\\]" (magpt--response->string '(1 2)))))

(ert-deftest magpt-sanitize-response-fences-and-prefix ()
  "sanitize-response should strip Answer:/Ответ: prefix; '=' fences are not stripped."
  (let* ((s "Answer: something\n=json\n{\"a\":1}\n=")
         (out (magpt--sanitize-response s)))
    (should (equal out "=json\n{\"a\":1}\n=")))
  (let* ((s "Ответ: см. ниже\n=\nhello\n=")
         (out (magpt--sanitize-response s)))
    (should (equal out "=\nhello\n="))))

(provide 'magpt-gpt-tests)
;;; magpt-gpt-tests.el ends here
