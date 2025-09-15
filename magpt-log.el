;;; magpt-log.el --- Shared logging for MaGPT  -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralized logger to avoid void-variable during partial reloads.
;; Keep this module lightweight and load it early where logging might run.

;;; Code:

(defcustom magpt-log-enabled t
  "If non-nil, write diagnostic logs to `magpt-log-buffer-name'."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-log-buffer-name "*magpt-log*"
  "Name of the buffer used for diagnostic logs."
  :type 'string
  :group 'magpt)

(defun magpt--log (fmt &rest args)
  "Append a diagnostic line to `magpt-log-buffer-name'."
  (when (and (boundp 'magpt-log-enabled) magpt-log-enabled)
    (let ((buf (get-buffer-create (if (boundp 'magpt-log-buffer-name)
                                      magpt-log-buffer-name
                                    "*magpt-log*"))))
      (with-current-buffer buf
        (goto-char (point-max))
        (let* ((ts (format-time-string "%Y-%m-%d %H:%M:%S"))
               (line (condition-case lerr
                         (apply #'format fmt args)
                       (error
                        (format "LOG-FMT-ERROR: fmt=%S args=%S err=%s"
                                fmt args (error-message-string lerr))))))
          (insert (format "[%s] %s\n" ts line)))))))

(defun magpt--backtrace-string ()
  "Return current backtrace as a string (best-effort)."
  (condition-case _
      (with-output-to-string (backtrace))
    (error "<no-backtrace>")))

(defun magpt--errstr (e)
  "Return a robust error message string for condition object E."
  (condition-case _
      (error-message-string e)
    (error (format "%S" e))))

;;;###autoload
(defun magpt-show-log ()
  "Open the magpt diagnostic log buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create magpt-log-buffer-name)))

(provide 'magpt-log)

;;; magpt-log.el ends here
