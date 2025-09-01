;;; test-helper.el --- Test helpers for magpt -*- lexical-binding: t; -*-

;; Common loader and fixtures for all test files.
;; To run a file:
;;   emacs -Q -batch -L . -L test -l test/test-helper.el \
;;     -l test/magpt-commit-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)

;; Make sure project root is on load-path when running from test/ dir.
(eval-and-compile
  (let* ((this-file (or load-file-name buffer-file-name))
         (test-dir (and this-file (file-name-directory this-file)))
         (proj-root (and test-dir (expand-file-name ".." test-dir))))
    (when (and proj-root (file-directory-p proj-root))
      (add-to-list 'load-path proj-root)
      (add-to-list 'load-path test-dir))))

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

(defun magpt-test--skip-unless-git ()
  "Skip the current test unless git is available."
  (unless (executable-find "git")
    (ert-skip "Git not found in PATH; skipping test.")))

(defun magpt-test--skip-unless-magit ()
  "Skip the current test unless Magit is available."
  (unless (featurep 'magit)
    (ert-skip "Magit is not installed; skipping test.")))

(defmacro magpt-test--with-temp-git-repo (&rest body)
  "Create a temporary git repository and execute BODY inside it.
Sets default-directory to the repo. Cleans up afterwards."
  (declare (indent 0))
  `(progn
     (magpt-test--skip-unless-git)
     (let* ((tmp (make-temp-file "magpt-repo" t))
            (default-directory (file-name-as-directory tmp)))
       (unwind-protect
           (progn
             ;; init and set identity to allow commits if needed
             (call-process (or (executable-find "git") "git") nil nil nil "init" "-q")
             (call-process (or (executable-find "git") "git") nil nil nil "config" "user.email" "test@example.org")
             (call-process (or (executable-find "git") "git") nil nil nil "config" "user.name" "Tester")
             ,@body)
         (ignore-errors (delete-directory tmp t))))))

(provide 'test-helper)

;;; test-helper.el ends here
