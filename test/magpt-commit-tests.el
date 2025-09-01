;;; magpt-commit-tests.el --- Commit-related tests -*- lexical-binding: t; -*-

;; To run:
;;   emacs -Q -batch -L . -L test -l test/test-helper.el \
;;     -l test/magpt-commit-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'magpt-commit)

;;; Truncation tests

(ert-deftest magpt-truncate-bytes-utf8-emoji ()
  "Ensure truncation respects UTF-8 boundaries with 4-byte emoji."
  (let* ((s "A😺B")            ;; bytes: 1 + 4 + 1 = 6
         (t5 (magpt--truncate-to-bytes s 5))
         (t4 (magpt--truncate-to-bytes s 4)))
    (should (equal t5 (substring s 0 2))) ;; "A😺" -> 1+4 = 5 bytes
    (should (equal t4 "A"))               ;; can't include the emoji at 4 bytes
    ;; maybe-truncate flags
    (let* ((res (magpt--maybe-truncate s 5)))
      (should (equal (car res) (substring s 0 2)))
      (should (eq (cdr res) t)))))

(ert-deftest magpt-truncate-bytes-utf8-cyrillic ()
  "Ensure truncation respects 2-byte Cyrillic chars."
  (let* ((s "AжB")            ;; bytes: 1 + 2 + 1 = 4
         (t3 (magpt--truncate-to-bytes s 3))
         (t2 (magpt--truncate-to-bytes s 2)))
    (should (equal t3 (substring s 0 2))) ;; "Aж" -> 1+2 = 3 bytes
    (should (equal t2 "A"))               ;; only ASCII fits
    (let* ((res (magpt--maybe-truncate s 10)))
      (should (equal (car res) s))
      (should (eq (cdr res) nil)))))

;;; Commit comment boundaries tests

(ert-deftest magpt-commit-boundaries-none ()
  "When there is no trailing comments block, MSG-END should be point-max."
  (magpt-test--with-commit-buffer "Summary\n\nBody line\n"
                                  (let ((git-commit-comment-char ?#))
                                    (cl-destructuring-bind (msg-end . comments-beg)
                                        (magpt--commit-message-boundaries)
                                      (should (eq comments-beg nil))
                                      (should (= msg-end (point-max)))))))

(ert-deftest magpt-commit-boundaries-trailing-comments ()
  "Detect a trailing comments block preceded by a blank line."
  (magpt-test--with-commit-buffer "Summary\n\nBody\n\n# C1\n# C2\n"
                                  (let ((git-commit-comment-char ?#))
                                    (save-excursion
                                      (goto-char (point-min))
                                      (re-search-forward "^# C1$")
                                      (let ((first-comment-bol (match-beginning 0)))
                                        (cl-destructuring-bind (msg-end . comments-beg)
                                            (magpt--commit-message-boundaries)
                                          (should (= msg-end first-comment-bol))
                                          (should (= comments-beg first-comment-bol))))))))

(ert-deftest magpt-commit-boundaries-early-comments-not-trailing ()
  "Earlier comment lines not forming the final trailing block must be ignored."
  (magpt-test--with-commit-buffer "Summary\n\n# inline note\nBody continues\n\n# T1\n# T2\n"
                                  (let ((git-commit-comment-char ?#))
                                    (save-excursion
                                      (goto-char (point-min))
                                      (re-search-forward "^# T1$")
                                      (let ((trailer-bol (match-beginning 0)))
                                        (cl-destructuring-bind (msg-end . comments-beg)
                                            (magpt--commit-message-boundaries)
                                          (should (= msg-end trailer-bol))
                                          (should (= comments-beg trailer-bol))))))))

;;; Insertion logic tests

(ert-deftest magpt-insert-into-empty-message-preserves-comments ()
  "Insert when top section is empty (only blank line before comments); preserve comments."
  (magpt-test--with-commit-buffer "\n# C1\n# C2\n"
                                  (let ((git-commit-comment-char ?#))
                                    (should (magpt--insert-into-commit-buffer-target (current-buffer) "New message"))
                                    (let ((final (buffer-string)))
                                      (should (string-prefix-p "New message\n" final))
                                      (should (string-match-p "^# C1" final))
                                      (should (string-match-p "^# C2" final))))))

(ert-deftest magpt-insert-replaces-existing-message-and-preserves-comments ()
  "Replace an existing message (confirmation required) and keep comments intact."
  (magpt-test--with-commit-buffer "Old message\n\n# C1\n# C2\n"
                                  (let ((git-commit-comment-char ?#))
                                    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                                      (should (magpt--insert-into-commit-buffer-target (current-buffer) "New"))
                                      (let ((final (buffer-string)))
                                        (should (string-prefix-p "New\n" final))
                                        (should (string-match-p "^# C1" final))
                                        (should (string-match-p "^# C2" final)))))))

(ert-deftest magpt-insert-cancelled-by-user ()
  "If user declines replacement, no changes should be made."
  (magpt-test--with-commit-buffer "Old message\n\n# C1\n"
                                  (let ((git-commit-comment-char ?#))
                                    (let ((before (buffer-string)))
                                      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
                                        (should-not (magpt--insert-into-commit-buffer-target (current-buffer) "New")))
                                      (should (equal before (buffer-string)))))))

;;; Callback tests

(ert-deftest magpt-callback-empty-response-cleans-overlay ()
  "Empty model response should not insert text; overlay is cleaned; message is logged."
  (magpt-test--with-commit-buffer "Summary\n\n# C1\n"
                                  (let ((git-commit-comment-char ?#))
                                    (magpt--show-commit-overlay (current-buffer))
                                    (should (overlayp magpt--commit-overlay))
                                    (let ((msgs (magpt-test--capture-message
                                                 (magpt--commit-callback "" (list :context (current-buffer))))))
                                      (let* ((expected (magpt--i18n 'empty-response)))
                                        (should (seq-some (lambda (s) (string-match-p (regexp-quote expected) s)) msgs))))
                                    (should (null magpt--commit-overlay)))))

;;; Prompt build tests

(ert-deftest magpt-build-commit-prompt-language-and-truncation-note ()
  "Prompt builder should add language directive and truncation note when applicable."
  (let ((magpt-commit-language "French"))
    (let* ((p (magpt--build-commit-prompt "TEMPLATE" "DIFF" t)))
      (should (string-match-p "Answer STRICTLY in French" p))
      (should (string-match-p "\\[diff truncated due to size limit\\]" p))))
  (let ((magpt-commit-language nil))
    (let ((p (magpt--build-commit-prompt "T" "D" nil)))
      (should-not (string-match-p "Answer STRICTLY in" p)))))

(provide 'magpt-commit-tests)
;;; magpt-commit-tests.el ends here
