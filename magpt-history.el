;;; magpt-history.el --- History storage for MaGPT  -*- lexical-binding: t; -*-
;;
;; Maintains history of AI requests/responses (shared for overview, actions, etc).
;; Session isolation: per-repo (Git root) lists with a global size limit across all repos.
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'json)

;; Core/gitrepo helpers
(declare-function magpt--project-root "magpt-git")

;; Back-compat: keep the legacy variable bound but unused
(defvar magpt--history-entries nil
  "Deprecated: legacy flat list of history entries (unused since per-root map was introduced).")

(defvar magpt--current-request nil
  "Dynamically bound prompt/request preview for history appends and AI overview rendering.")

(defcustom magpt-history-max-entries 200
  "Global maximum number of entries to keep across ALL repositories.
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

;; Internal storage: per-root map and a global queue to enforce a total cap.
(defvar magpt--history-map (make-hash-table :test 'equal)
  "Map: ROOT → newest-first list of entry plists.")

(defvar magpt--history-global-queue nil
  "Queue of (ROOT . SEQ) pairs, oldest to newest, used for global trimming.")

(defvar magpt--history-seq 0
  "Monotonic counter assigned to entries as :seq to support trimming.")

(defun magpt--history--normalize-root (root)
  "Normalize ROOT to a canonical directory string (truename, trailing slash)."
  (when (and root (stringp root))
    (file-name-as-directory (file-truename root))))

(defun magpt--history-current-root ()
  "Return normalized Git root for the current context or signal an error."
  (let ((r (ignore-errors (magpt--project-root))))
    (unless (and (stringp r) (> (length r) 0))
      (user-error "No Git repository found for current directory"))
    (magpt--history--normalize-root r)))

(defun magpt--history--total-count ()
  "Return total number of entries across all roots."
  (length magpt--history-global-queue))

(defun magpt--history--trim-global ()
  "Trim oldest entries until total count ≤ `magpt-history-max-entries'."
  (when (and (integerp magpt-history-max-entries)
             (> magpt-history-max-entries 0))
    (while (> (magpt--history--total-count) magpt-history-max-entries)
      (let* ((old (car (last magpt--history-global-queue))))
        (setq magpt--history-global-queue (butlast magpt--history-global-queue))
        (when (consp old)
          (let* ((root (car old))
                 (seq-id (cdr old))
                 (lst (gethash root magpt--history-map)))
            (when lst
              (setf (gethash root magpt--history-map)
                    (cl-remove-if (lambda (e) (eq (plist-get e :seq) seq-id)) lst)))))))))

(defun magpt--history--fallback-refresh-magit (root)
  "Fallback: мягко обновить видимые magit-status ТОЛЬКО для ROOT, если нет подписчиков."
  (when (featurep 'magit)
    (run-at-time 0 nil
                 (lambda ()
                   (dolist (win (window-list))
                     (with-current-buffer (window-buffer win)
                       (when (derived-mode-p 'magit-status-mode)
                         (let* ((buf-root (ignore-errors (magpt--history--normalize-root (magpt--project-root)))))
                           (when (or (null root) (and buf-root (string= buf-root root)))
                             (condition-case _
                                 (progn
                                   (when (fboundp 'magit-refresh) (magit-refresh))
                                   (when (fboundp 'magit-refresh-buffer) (magit-refresh-buffer)))
                               (error nil)))))))))))

(defun magpt--history-append-entry (task request response &optional note &rest kvs)
  "Append an entry to history for TASK and run update hooks (with ROOT arg).
KVS extends the stored plist."
  (let* ((root (magpt--history-current-root))
         (resp-raw (if (stringp response) response (prin1-to-string response)))
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
                      :note note
                      :seq (cl-incf magpt--history-seq))))
    (when kvs (setq entry (append entry kvs)))
    ;; Insert newest-first into per-root list
    (let ((lst (gethash root magpt--history-map)))
      (setf (gethash root magpt--history-map) (cons entry lst)))
    ;; Append to global queue (oldest..newest)
    (setq magpt--history-global-queue
          (append magpt--history-global-queue (list (cons root (plist-get entry :seq)))))
    (when (fboundp 'magpt--log)
      (magpt--log "history: append root=%s task=%s valid=%s note=%s total=%d"
                  root task (if json-valid "t" "nil") (or note "")
                  (magpt--history--total-count)))
    ;; Trim globally if needed
    (magpt--history--trim-global)
    ;; Fire hook with ROOT argument; if no subscribers, fallback-refresh ROOT only.
    (let* ((have-hook (and (boundp 'magpt-history-changed-hook)
                           (symbol-value 'magpt-history-changed-hook))))
      (condition-case eh
          (when (boundp 'magpt-history-changed-hook)
            (run-hook-with-args 'magpt-history-changed-hook root))
        (error
         (when (fboundp 'magpt--log)
           (magpt--log "history: changed-hook error: %s"
                       (if (fboundp 'magpt--errstr)
                           (magpt--errstr eh)
                         (error-message-string eh))))))
      (when (null have-hook)
        (ignore-errors (magpt--history--fallback-refresh-magit root))))))

(defun magpt--history-entries-for-root (root)
  "Return newest-first list of entries for ROOT (normalized)."
  (let* ((key (magpt--history--normalize-root root)))
    (or (gethash key magpt--history-map) '())))

(defun magpt--history-tasks (&optional root)
  "Return a list of unique task symbols present in ROOT (current root by default)."
  (let* ((r (or root (ignore-errors (magpt--history-current-root))))
         (lst (magpt--history-entries-for-root r)))
    (delete-dups (mapcar (lambda (e) (plist-get e :task)) lst))))

(defun magpt--history-last-entry-for (task &optional root)
  "Return the most recent history entry plist for TASK in ROOT (current root by default)."
  (let* ((r (or root (ignore-errors (magpt--history-current-root))))
         (lst (magpt--history-entries-for-root r)))
    (seq-find (lambda (e) (eq (plist-get e :task) task)) lst)))

(defun magpt--history-last-any (&optional root)
  "Return the newest entry for ROOT (current root by default), or nil."
  (let* ((r (or root (ignore-errors (magpt--history-current-root))))
         (lst (magpt--history-entries-for-root r)))
    (car lst)))

(provide 'magpt-history)

;;; magpt-history.el ends here
