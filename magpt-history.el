;;; magpt-history.el --- History storage for MaGPT  -*- lexical-binding: t; -*-
;;
;; Maintains history of AI requests/responses (shared for overview, actions, etc).
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'json)

(defvar magpt--history-entries nil
  "List of history entries (plists):
  :time STRING :task SYMBOL :request STRING :response STRING
  :valid t/nil :note STRING (optional).")

(defvar magpt--current-request nil
  "Dynamically bound prompt/request preview for history appends and AI overview rendering.")

(defcustom magpt-history-max-entries 200
  "Maximum number of entries to keep in history for AI Overview and actions.
If nil or non-positive, history is unbounded."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Max entries"))
  :group 'magpt)

(defun magpt--entry-parse-json-safe (entry)
  "Parse ENTRY's :response as JSON; return alist or nil."
  (let ((resp (plist-get entry :response)))
    (condition-case _err
        (json-parse-string (or resp "") :object-type 'alist :array-type 'list)
      (error nil))))

;; Forward declaration for sanitizer
(declare-function magpt--sanitize-response "magpt-gpt" (s))

(defun magpt--history--fallback-refresh-magit ()
  "Fallback: мягко обновить видимые magit-status, если нет подписчиков на хук."
  (when (featurep 'magit)
    (run-at-time 0 nil
                 (lambda ()
                   (dolist (win (window-list))
                     (with-current-buffer (window-buffer win)
                       (when (derived-mode-p 'magit-status-mode)
                         (condition-case _
                             (progn
                               (when (fboundp 'magit-refresh) (magit-refresh))
                               (when (fboundp 'magit-refresh-buffer) (magit-refresh-buffer)))
                           (error nil)))))))))

(defun magpt--history-append-entry (task request response &optional note &rest kvs)
  "Append an entry to history for TASK and run update hooks.
Extra KV pairs can be provided in KVS to extend the stored plist."
  (let* ((resp-raw (if (stringp response) response (prin1-to-string response)))
         (resp
          (if (fboundp 'magpt--sanitize-response)
              (magpt--sanitize-response (string-trim (or resp-raw "")))
            (string-trim (or resp-raw ""))))
         (looks-like-json (string-match-p "\\`[ \t\n]*[{\\[]" resp))
         (json-valid
          (and looks-like-json
               (condition-case _err
                   (progn (json-parse-string resp :object-type 'alist) t)
                 (error nil))))
         (entry (list :time (format-time-string "%Y-%m-%d %H:%M:%S")
                      :task task
                      :request (or request "")
                      :response resp
                      :valid json-valid
                      :note note)))
    ;; Extend entry with any extra kvs (e.g., :status-snapshot).
    (when kvs (setq entry (append entry kvs)))
    (push entry magpt--history-entries)
    (when (fboundp 'magpt--log)
      (magpt--log "history: append task=%s valid=%s note=%s total=%d"
                  task (if json-valid "t" "nil") (or note "") (length magpt--history-entries)))
    ;; Trim history if needed.
    (when (and (integerp magpt-history-max-entries)
               (> magpt-history-max-entries 0)
               (> (length magpt--history-entries) magpt-history-max-entries))
      (setcdr (nthcdr (1- magpt-history-max-entries) magpt--history-entries) nil))
    ;; Fire notification hook if defined in parent.
    (when (boundp 'magpt-history-changed-hook)
      (let* ((n (length (and (boundp 'magpt-history-changed-hook)
                             (symbol-value 'magpt-history-changed-hook)))))
        (when (fboundp 'magpt--log)
          (magpt--log "history: run-changed-hook fns=%d" n))
        (condition-case eh
            (run-hooks 'magpt-history-changed-hook)
          (error
           (when (fboundp 'magpt--log)
             (magpt--log "history: changed-hook error: %s"
                         (if (fboundp 'magpt--errstr)
                             (magpt--errstr eh)
                           (error-message-string eh))))))
        ;; Если нет подписчиков (n=0), аккуратно обновим magit-status сами.
        (when (zerop n)
          (ignore-errors (magpt--history--fallback-refresh-magit)))))))

(defun magpt--history-tasks ()
  "Return a list of unique task symbols present in history."
  (delete-dups (mapcar (lambda (e) (plist-get e :task)) magpt--history-entries)))

(defun magpt--history-last-entry-for (task)
  "Return the most recent history entry plist for TASK, or nil if none."
  (let ((lst (and (boundp 'magpt--history-entries) magpt--history-entries)))
    (seq-find (lambda (e) (eq (plist-get e :task) task))
              lst)))

(provide 'magpt-history)

;;; magpt-history.el ends here
