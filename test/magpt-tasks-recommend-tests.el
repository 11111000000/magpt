;;; magpt-tasks-recommend-tests.el --- Phase 2 tasks tests (subset) -*- lexical-binding: t; -*-

;; To run:
;;   emacs -Q -batch -L . -L test -l test/test-helper.el \
;;     -l test/magpt-tasks-recommend-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'magpt-tasks-recommend)

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

(provide 'magpt-tasks-recommend-tests)
;;; magpt-tasks-recommend-tests.el ends here
