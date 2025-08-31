;;; magpt-gpt.el --- GPT/gptel wrappers for MaGPT  -*- lexical-binding: t; -*-

;; Author: Peter
;; Package-Requires: ((emacs "28.1") (gptel "0.9"))
;; Keywords: tools, ai
;; URL: https://github.com/11111000000/magpt

;;; Commentary:
;; Centralize gptel usage, response sanitization, and safe callback wrapping.

;;; Code:

(require 'subr-x)
(require 'gptel)
(require 'json)

;; External deps from core (magpt.el)
(declare-function magpt--log "ext:magpt" (fmt &rest args))
(declare-function magpt--backtrace-string "ext:magpt")
(declare-function magpt--i18n "ext:magpt" (key &rest args))

(defvar magpt-model)
(defvar gptel-model)
(defvar magpt-log-enabled)
(defvar magpt-info-language)
(defvar magpt-commit-language)

(defun magpt--response->string (resp)
  "Return RESP as a string, tolerating backend variations."
  (cond
   ((stringp resp) resp)
   ((and (listp resp) (assq 'content resp))
    (let ((c (cdr (assq 'content resp))))
      (if (stringp c) c (format "%S" resp))))
   ((hash-table-p resp)
    (condition-case _ (json-encode resp) (error (format "%S" resp))))
   ((listp resp)
    (condition-case _ (json-encode resp) (error (format "%S" resp))))
   (t (format "%S" resp))))

(defun magpt--sanitize-response (s)
  "Sanitize LLM response S: strip common Markdown fences and leading labels."
  (let* ((s (string-trim (or s ""))))
    ;; Strip leading 'Answer:'/'Ответ:' labels (common model prefixes)
    (setq s (replace-regexp-in-string "\\`\\(?:\\s-*\\(Answer\\|Ответ\\)[:：].*\\n+\\)+" "" s))
    ;; Strip outer Markdown fences (=lang ... = or ~~~)
    (let* ((lines (split-string s "\n"))
           (first (car lines))
           (last  (car (last lines))))
      (when (and first last
                 (string-match-p "\\`[`~]\\{3,\\}" first)
                 (string-match-p "\\`[`~]\\{3,\\}[ \t]*\\'" last))
        (setq s (mapconcat #'identity (butlast (cdr lines)) "\n"))))
    s))

(defun magpt--safe-callback (cb)
  "Wrap CB to prevent hard failures; log diagnostics on error."
  (lambda (resp info)
    (when magpt-log-enabled
      (let* ((ty (type-of resp))
             (preview (condition-case _
                          (let* ((s (magpt--response->string resp))
                                 (n (min 180 (length s))))
                            (substring s 0 n))
                        (error "<unprintable>"))))
        (magpt--log "safe-callback: resp-type=%S preview=%s info=%S"
                    ty preview (and (listp info)
                                    (ignore-errors
                                      (cl-subseq info 0 (min 10 (length info))))))))
    (condition-case err
        (funcall cb resp info)
      (error
       (magpt--log "callback exception: %s" (error-message-string err))
       (magpt--log "callback exception: signal=%S data=%S" (car-safe err) (cdr-safe err))
       (magpt--log "callback exception: BT:\n%s" (magpt--backtrace-string))
       (message "%s" (magpt--i18n 'callback-error (error-message-string err)))))))

(defun magpt--gptel-request (prompt &rest args)
  "Call `gptel-request' with PROMPT and ARGS, adding logging and safe callback."
  (let* ((plist args)
         (cb (plist-get plist :callback))
         (stream (plist-get plist :stream)))
    (when magpt-log-enabled
      (magpt--log "gptel-request: model=%S stream=%S prompt-len=%d preview=%s"
                  (or magpt-model gptel-model)
                  stream
                  (length (or prompt ""))
                  (let* ((p (or prompt "")) (n (min 180 (length p))))
                    (substring p 0 n))))
    (when cb
      (setq plist (plist-put plist :callback (magpt--safe-callback cb))))
    (apply #'gptel-request prompt plist)))

(defun magpt--system-prompt (kind)
  "Return a strict system directive based on language and KIND ('commit or 'info)."
  (let* ((lang (pcase kind
                 ('commit magpt-commit-language)
                 (_      magpt-info-language)))
         (l (and (stringp lang) (> (length lang) 0) lang)))
    (when l
      (format "Answer STRICTLY in %s. Do not use any other language." l))))

(provide 'magpt-gpt)

;;; magpt-gpt.el ends here
