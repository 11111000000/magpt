;;; magpt-transient-tests.el --- Transient aggregator tests -*- lexical-binding: t; -*-

;; To run:
;;   emacs -Q -batch -L . -L test -l test/test-helper.el \
;;     -l test/magpt-transient-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'magpt-transient)

(ert-deftest magpt-transient-normalize-suggestions ()
  "magpt--ai--normalize-suggestions should convert JSON alist to plist list."
  (let* ((data '((summary . "S")
                 (suggestions . (((title . "T1")
                                  (commands . ("git status" "git add ."))
                                  (magit_keys . ("s" "S")))
                                 ((title . "T2")
                                  (commands . ("echo hi"))
                                  (keys . ("x"))))))))
    (let ((out (magpt--ai--normalize-suggestions data)))
      (should (listp out))
      (should (= (length out) 2))
      (let* ((first (car out)))
        (should (string= (plist-get first :title) "T1"))
        (should (string-match-p "git status" (plist-get first :commands)))
        (should (equal (plist-get first :keys) '("s" "S")))))))

(ert-deftest magpt-transient-latest-entry-among-tasks ()
  "AI Actions aggregator should pick the newest entry among configured tasks."
  (let ((magpt--history-entries nil)
        (magpt-ai-actions-source-tasks '(explain-status explain-branches)))
    ;; Build history with newest-first ordering.
    (push (list :time "2024-01-01 11:00:00"
                :task 'explain-status
                :request ""
                :response "{\"summary\":\"old\",\"suggestions\":[{\"title\":\"A\",\"commands\":[\"git a\"],\"keys\":[\"x\"]}]}"
                :valid t)
          magpt--history-entries)
    (push (list :time "2024-01-01 12:00:00"
                :task 'explain-branches
                :request ""
                :response "{\"summary\":\"new\",\"suggestions\":[{\"title\":\"B\",\"commands\":[\"git b\"],\"keys\":[\"y\"]}]}"
                :valid t)
          magpt--history-entries)
    (let* ((sugs (magpt--ai-suggestions-from-last-explain-status)))
      (should (listp sugs))
      (should (= (length sugs) 1))
      (should (string= (plist-get (car sugs) :title) "B"))
      (should (string= magpt--ai-actions-summary "new")))))

(provide 'magpt-transient-tests)
;;; magpt-transient-tests.el ends here
