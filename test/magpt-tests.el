;;; magpt-tests.el --- ERT tests for magpt  -*- lexical-binding: t; -*-

;; To run:
;;   emacs -Q -batch -L . -l test/magpt-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)

;; Make sure project root is on load-path when running from test/ dir.
(eval-and-compile
  (let* ((this-file (or load-file-name buffer-file-name))
         (test-dir (and this-file (file-name-directory this-file)))
         (proj-root (and test-dir (expand-file-name ".." test-dir))))
    (when (and proj-root (file-directory-p proj-root))
      (add-to-list 'load-path proj-root))))

;; Stub `gptel' when running tests without the package installed.
(unless (featurep 'gptel)
  (defvar gptel-model nil)
  (defun gptel-request (&rest _args)
    "Test stub for gptel-request. Should not be called in unit tests."
    (error "gptel stub called"))
  (provide 'gptel))

(require 'magpt)

(defconst magpt-test--project-root
  (let* ((this-file (or load-file-name buffer-file-name))
         (test-dir (and this-file (file-name-directory this-file))))
    (file-name-as-directory (expand-file-name ".." test-dir))))

(defmacro magpt-test--with-commit-buffer (content &rest body)
  "Create a temporary buffer that looks like a Git commit message buffer.
CONTENT is inserted, buffer-file-name is set to COMMIT_EDITMSG to satisfy
`magpt--commit-buffer-p'. BODY is executed in that buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (setq buffer-file-name "COMMIT_EDITMSG")
     (insert ,content)
     (goto-char (point-min))
     ,@body))

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

;;; Project root detection tests

(ert-deftest magpt-project-root-detection ()
  "magpt--project-root should return the same as rev-parse --show-toplevel."
  (if (not (magpt--executable-git))
      (ert-skip "Git not found in PATH; skipping project root test.")
    (let ((default-directory magpt-test--project-root))
      (let ((expected (magpt--git-root-from default-directory)))
        (should (stringp expected))
        (dolist (strategy '(prefer-magit prefer-vc prefer-project))
          (let ((magpt-project-root-strategy strategy))
            (should (equal (magpt--project-root) expected))))))))

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
                                    ;; Existing (trimmed) message is empty, so no prompt should occur.
                                    (should (magpt--insert-into-commit-buffer-target (current-buffer) "New message"))
                                    (let ((final (buffer-string)))
                                      ;; Should have new message at top, comments preserved.
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
                                        ;; The blank line before comments may be removed; ensure comments remain and new text at top.
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

;; Utilities for capturing `message' output in tests.
(defmacro magpt-test--capture-message (&rest body)
  "Execute BODY while capturing calls to `message'. Return list of messages."
  (declare (indent 0))
  `(let (acc)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (if (null fmt)
                      nil
                    (let ((str (apply #'format fmt args)))
                      (push str acc)
                      str)))))
       ,@body)
     (nreverse acc)))

(ert-deftest magpt-callback-empty-response-cleans-overlay ()
  "Empty model response should not insert text; overlay is cleaned; message is logged."
  (magpt-test--with-commit-buffer "Summary\n\n# C1\n"
                                  (let ((git-commit-comment-char ?#))
                                    ;; Prepare overlay
                                    (magpt--show-commit-overlay (current-buffer))
                                    (should (overlayp magpt--commit-overlay))
                                    ;; Run callback with empty response
                                    (let ((msgs (magpt-test--capture-message
                                                 (magpt--commit-callback "" (list :context (current-buffer))))))
                                      (let* ((expected (magpt--i18n 'empty-response)))
                                        (should (seq-some (lambda (s) (string-match-p (regexp-quote expected) s)) msgs))))
                                    ;; Overlay must be removed
                                    (should (null magpt--commit-overlay)))))



;;; Panel/request recording tests

(ert-deftest magpt-history-records-request-preview ()
  "History entry should store request preview and validate JSON."
  (let ((magpt--history-entries nil))
    (magpt--history-append-entry 'explain-status "PROMPT-X" "{\"x\":1}")
    (let ((e (car magpt--history-entries)))
      (should (equal (plist-get e :request) "PROMPT-X"))
      (should (equal (plist-get e :response) "{\"x\":1}"))
      ;; Bare JSON string or object/array should be considered valid JSON by parser.
      (should (eq (plist-get e :valid) t)))))





;;; Phase 2 tests



(ert-deftest magpt-ctx-hunk-or-region-region-basic ()
  "Region context should return :kind 'region, non-empty text and positive byte size."
  (let ((tmp (make-temp-file "magpt" nil ".txt")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name tmp)
          (insert "line1\nline2\n")
          (goto-char (point-min))
          (set-mark (point))      ; mark at bol of line1
          (goto-char (line-end-position)) ; region: first line
          (activate-mark)
          (let* ((res (magpt--ctx-hunk-or-region nil))
                 (data (nth 0 res))
                 (_preview (nth 1 res))
                 (bytes (nth 2 res)))
            (should (eq (plist-get data :kind) 'region))
            (should (string-match-p "line1" (or (plist-get data :text) "")))
            (should (and (integerp bytes) (> bytes 0)))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest magpt-apply-stage-intent-executes-ops ()
  "Applying a simple stage-by-intent plan should call git add/restore with expected args."
  (let ((magpt--history-entries nil)
        (magpt-allow-apply-safe-ops t)
        (magpt-test--git-calls nil))
    ;; History entry with JSON plan
    (push (list :time "T"
                :task 'stage-by-intent
                :request ""
                :response
                "{\"groups\":[{\"title\":\"g1\",\"rationale\":\"x\",\"files\":[{\"path\":\"a.txt\",\"action\":\"stage\"},{\"path\":\"b.txt\",\"action\":\"unstage\"}]}]}")
          magpt--history-entries)
    ;; Stub confirm and git
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'magpt--project-root) (lambda () default-directory))
              ((symbol-function 'magpt--git)
               (lambda (_dir &rest args)
                 (push args magpt-test--git-calls)
                 "")))
      (magpt--apply-stage-by-intent-last)
      (let ((calls (nreverse magpt-test--git-calls)))
        (should (equal calls
                       '(("add" "--" "a.txt")
                         ("restore" "--staged" "--" "b.txt"))))))))

(provide 'magpt-tests)

;;; magpt-tests.el ends here
