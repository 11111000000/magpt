;;; magpt-apply-restore-tests.el --- Restore-file buttons tests -*- lexical-binding: t; -*-

;; To run:
;;   emacs -Q -batch -L . -L test -l test/test-helper.el \
;;     -l test/magpt-apply-restore-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'magpt-apply)

(defun magpt-test--make-entry (&rest kvs)
  "Helper to build a minimal history entry plist for restore-file-suggest."
  (apply #'list :task 'restore-file-suggest :valid t kvs))

(ert-deftest magpt-apply-restore-file-apply-both ()
  "Restore file Apply should call git restore for index and worktree when 'both' selected."
  (let ((magpt-allow-apply-safe-ops t)
        (calls nil))
    (let* ((entry (magpt-test--make-entry :target-path "a.txt" :target-rev "HEAD"))
           (btn (let ((buf (generate-new-buffer " *magpt-btn*")))
                  (with-current-buffer buf
                    (insert "xx")
                    (make-text-button 1 2 'magpt-entry entry)
                    (button-at 1)))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ;; Avoid reading path/rev interactively
                ((symbol-function 'magpt--read-restore-file-args)
                 (lambda (_entry)
                   (list default-directory "a.txt" "HEAD")))
                ;; No stash prompt during test
                ((symbol-function 'magpt--path-has-unstaged-p)
                 (lambda (&rest _) nil))
                ;; Choose destination 'both'
                ((symbol-function 'completing-read)
                 (lambda (prompt &rest _)
                   (if (string-match-p "Restore to" prompt) "both" "IGNORED")))
                ;; Capture git calls without executing
                ((symbol-function 'magpt--git)
                 (lambda (_dir &rest args)
                   (push args calls)
                   "")))
        (magpt--btn-restore-file-apply btn)
        (setq calls (nreverse calls))
        (should (= (length calls) 2))
        (should (equal (nth 0 calls) '("restore" "--source" "HEAD" "--staged" "--" "a.txt")))
        (should (equal (nth 1 calls) '("restore" "--source" "HEAD" "--" "a.txt")))))))

(ert-deftest magpt-apply-restore-file-preview-diff ()
  "Preview should prefer diff for REV:PATH and fall back to blob if diff empty."
  (let* ((entry (magpt-test--make-entry :target-path "a.txt" :target-rev "HEAD"))
         (btn (let ((buf (generate-new-buffer " *magpt-btn*")))
                (with-current-buffer buf
                  (insert "xx")
                  (make-text-button 1 2 'magpt-entry entry)
                  (button-at 1))))
         (captured nil))
    (cl-letf (((symbol-function 'magpt--read-restore-file-args)
               (lambda (_entry)
                 (list default-directory "a.txt" "HEAD")))
              ;; Return a fake diff to ensure diff branch is used
              ((symbol-function 'magpt--git)
               (lambda (_dir &rest args)
                 (cond
                  ((equal (car args) "diff") "---\n@@\n- old\n+ new\n")
                  (t ""))))
              ;; Capture preview open
              ((symbol-function 'magpt--btn-preview-text)
               (lambda (title text &optional _mode)
                 (setq captured (list title text))
                 (get-buffer-create "*dummy*"))))
      (magpt--btn-restore-file-preview btn)
      (should (listp captured))
      (should (string-match-p "diff: a.txt @ HEAD" (car captured)))
      (should (string-match-p "\\+ new" (cadr captured))))))

(provide 'magpt-apply-restore-tests)
;;; magpt-apply-restore-tests.el ends here
