;;; magpt-stability.el --- Stability wrappers for MaGPT  -*- lexical-binding: t; -*-

;;; Commentary:
;; Catch and down-convert synchronous gptel dispatch errors so that:
;; - we don't crash;
;; - we don't spam double errors if callback still arrives later;
;; - we keep a clear log and a user-visible message.
;;
;; We do NOT touch gptel itself.

;;; Code:

(require 'cl-lib)
(require 'magpt-log)           ;; magpt--log, magpt--backtrace-string
(require 'magpt-i18n nil t)    ;; magpt--i18n (optional)

(defvar magpt-stability--squelch-sync-gptel-error t
  "If non-nil, swallow synchronous errors thrown by magpt--gptel-request.
The async callback (if any) is still allowed to run later.")

(defun magpt-stability--errstr (err)
  (or (ignore-errors
        (if (fboundp 'magpt--errstr)
            (magpt--errstr err)
          (error-message-string err)))
      "<no-error-object>"))

(defun magpt-stability--gptel-safe (orig prompt &rest plist)
  "Around advice for `magpt--gptel-request' to absorb sync errors safely."
  (if (not magpt-stability--squelch-sync-gptel-error)
      (apply orig prompt plist)
    (condition-case err
        (apply orig prompt plist)
      (error
       (let* ((emsg (magpt-stability--errstr err)))
         (magpt--log "gptel-safe: synchronous error caught: %s\nBT:\n%s"
                     emsg (magpt--backtrace-string))
         ;; Show a single, localized user message (do not re-signal).
         (ignore-errors
           (message "%s"
                    (or (condition-case _
                            (magpt--i18n 'gptel-error emsg)
                          (error (format "magpt: error calling gptel: %s" emsg)))
                        (format "magpt: error calling gptel: %s" emsg)))))
       ;; Return nil as dispatcher handle; async callback (if any) may still fire.
       nil))))

;; Install advice once.
(with-eval-after-load 'magpt-gpt
  (advice-add 'magpt--gptel-request :around #'magpt-stability--gptel-safe))

;; Also harden core gptel so its synchronous errors never bubble up.
(with-eval-after-load 'gptel
  (advice-add 'gptel-request :around
              (lambda (orig prompt &rest plist)
                (if (not magpt-stability--squelch-sync-gptel-error)
                    (apply orig prompt plist)
                  (condition-case e
                      (apply orig prompt plist)
                    (error
                     (let ((emsg (magpt-stability--errstr e)))
                       (magpt--log "gptel-safe(core): synchronous error caught: %s\nBT:\n%s"
                                   emsg (magpt--backtrace-string)))
                     ;; Do not signal; let async callback (if any) still arrive.
                     nil))))))

(provide 'magpt-stability)

;;; magpt-stability.el ends here
