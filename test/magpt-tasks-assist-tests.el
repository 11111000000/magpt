;;; magpt-tasks-assist-tests.el --- Assist tasks tests (subset) -*- lexical-binding: t; -*-

;; To run:
;;   emacs -Q -batch -L . -L test -l test/test-helper.el \
;;     -l test/magpt-tasks-assist-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'magpt-tasks-assist)

(ert-deftest magpt-porcelain-parse-basic ()
  "Parse porcelain into staged/unstaged/untracked sets."
  (let* ((porc " M a.txt\nA  b.txt\n?? x\n")
         (res (magpt--porcelain-parse porc)))
    (should (equal (plist-get res :staged) '("b.txt")))
    (should (equal (plist-get res :unstaged) '("a.txt")))
    (should (equal (plist-get res :untracked) '("x")))))

(ert-deftest magpt-prompt-explain-status-includes-keys-and-facts ()
  "Explain-status prompt should include MAGIT KEYS block and FACTS when provided."
  (let* ((data (list :status " M a.txt\n?? x"
                     :recent-git-output "fatal: something"
                     :magit-keys "- Stage selection: s"
                     :staged-files '("b.txt")
                     :unstaged-files '("a.txt")
                     :untracked-files '("x")))
         (p (magpt--prompt-explain-status data)))
    (should (string-match-p "BEGIN MAGIT KEYS HELP" p))
    (should (string-match-p "STAGED FILES" p))
    (should (string-match-p "UNSTAGED FILES" p))
    (should (string-match-p "RECENT GIT OUTPUT" p))
    (should (string-match-p "--- BEGIN STATUS ---" p))))

(ert-deftest magpt-ctx-restore-file-suggest-direct-ctx ()
  "Context for restore-file-suggest should use given :path and :rev without prompting."
  (cl-letf (((symbol-function 'magpt--format-magit-keys-cheatsheet-safe) (lambda () "")))
    (let* ((res (magpt--ctx-restore-file-suggest (list :path "a.txt" :rev "HEAD")))
           (data (nth 0 res))
           (preview (nth 1 res)))
      (should (equal (plist-get data :path) "a.txt"))
      (should (equal (plist-get data :rev) "HEAD"))
      (should (string-match-p "FILE: a.txt" preview))
      (should (string-match-p "REV: HEAD" preview)))))

(ert-deftest magpt-prompt-push-pull-shape ()
  "Push/Pull prompt should include status, upstream and remotes sections."
  (let* ((data (list :status "## main...origin/main [ahead 1]"
                     :upstream "origin/main" :ahead "1" :behind "0"
                     :remote "origin\thttps://example.git (fetch)"
                     :recent "" :magit-keys "")))
    (let ((p (magpt--prompt-explain-push-pull data)))
      (should (string-match-p "--- STATUS -sb ---" p))
      (should (string-match-p "--- UPSTREAM ---" p))
      (should (string-match-p "--- REMOTES ---" p)))))

;; P2 tasks — prompt shape/tests

(ert-deftest magpt-prompt-reset-files-includes-status-marker ()
  "Reset-files prompt should include porcelain status section."
  (let* ((data (list :porcelain " M a.txt\n" :magit-keys "")))
    (let ((p (magpt--prompt-reset-files-suggest data)))
      (should (string-match-p "--- STATUS (porcelain) ---" p))
      (should (string-match-p " M a.txt" p)))))

(ert-deftest magpt-prompt-undo-commits-sections ()
  "Undo-commits prompt should include all required sections."
  (let* ((data (list :status "## main" :upstream "origin/main"
                     :ahead-behind "0 1" :log "c1\nc2" :magit-keys "")))
    (let ((p (magpt--prompt-undo-commits data)))
      (should (string-match-p "--- STATUS -sb ---" p))
      (should (string-match-p "--- UPSTREAM ---" p))
      (should (string-match-p "--- AHEAD/BEHIND ---" p))
      (should (string-match-p "--- LOG ---" p)))))

(ert-deftest magpt-prompt-reflog-rescue-sections ()
  "Reflog-rescue prompt should include reflog section."
  (let* ((data (list :reflog "abc HEAD@{0} msg" :magit-keys "")))
    (let ((p (magpt--prompt-reflog-rescue data)))
      (should (string-match-p "--- REFLOG ---" p))
      (should (string-match-p "HEAD@{0}" p)))))

(ert-deftest magpt-prompt-stash-sections ()
  "Stash prompt should include status and stash list sections."
  (let* ((data (list :status " M a.txt" :stash "stash@{0}: WIP" :magit-keys "")))
    (let ((p (magpt--prompt-stash data)))
      (should (string-match-p "--- STATUS ---" p))
      (should (string-match-p "--- STASH LIST ---" p)))))

(ert-deftest magpt-prompt-detached-head-sections ()
  "Detached HEAD prompt should include branch and last commit sections."
  (let* ((data (list :branch "HEAD" :last "abc initial" :magit-keys "")))
    (let ((p (magpt--prompt-detached-head data)))
      (should (string-match-p "--- BRANCH ---" p))
      (should (string-match-p "--- LAST COMMIT ---" p)))))

(ert-deftest magpt-prompt-set-upstream-sections ()
  "Set-upstream prompt should include status, branch, upstream and remotes."
  (let* ((data (list :status "## main" :branch "main"
                     :upstream "origin/main" :remote "origin git@example"
                     :magit-keys "")))
    (let ((p (magpt--prompt-set-upstream data)))
      (should (string-match-p "--- STATUS -sb ---" p))
      (should (string-match-p "--- BRANCH ---" p))
      (should (string-match-p "--- UPSTREAM ---" p))
      (should (string-match-p "--- REMOTES ---" p)))))

(provide 'magpt-tasks-assist-tests)
;;; magpt-tasks-assist-tests.el ends here
