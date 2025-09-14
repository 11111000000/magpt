;;; run-all.el --- Load all magpt tests -*- lexical-binding: t; -*-

;; Example:
;;   emacs -Q -batch -L . -L test -l test/run-all.el -f ert-run-tests-batch-and-exit

(load (expand-file-name "test-helper.el" (file-name-directory (or load-file-name buffer-file-name))))

(require 'magpt-commit-tests)
(require 'magpt-git-tests)
(require 'magpt-history-tests)
(require 'magpt-apply-tests)
(require 'magpt-gpt-tests)
(require 'magpt-magit-overview-tests)
(require 'magpt-tasks-recommend-tests)
(require 'magpt-transient-tests)



(provide 'run-all)
;;; run-all.el ends here
