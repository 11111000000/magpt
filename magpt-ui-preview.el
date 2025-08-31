;;; magpt-ui-preview.el --- UI preview helpers for MaGPT  -*- lexical-binding: t; -*-

;; Author: Peter
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, vc, git, ai
;; URL: https://github.com/11111000000/magpt

;;; Commentary:
;; Preview buffer helpers (read-only popup with optional major-mode).

;;; Code:

(require 'subr-x)

(defun magpt--btn-preview-text (title text &optional mode)
  "Open TEXT in a preview buffer with TITLE; MODE selects major-mode symbol."
  (let ((buf (get-buffer-create "*magpt-preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-trim-right text) "\n")
        (goto-char (point-min))
        (cond
         ((eq mode 'json) (when (fboundp 'json-mode) (ignore-errors (json-mode)))
          (condition-case _ (when (fboundp 'json-pretty-print-buffer)
                              (json-pretty-print-buffer))
            (error nil)))
         ((eq mode 'shell) (ignore-errors (sh-mode)))
         (t (text-mode)))
        (setq buffer-read-only t)))
    (pop-to-buffer buf)
    (rename-buffer (format "*magpt-preview: %s*" title) t)))

(provide 'magpt-ui-preview)

;;; magpt-ui-preview.el ends here
