;;; magpt-transient.el --- Transient/Magit integration for MaGPT  -*- lexical-binding: t; -*-

;; Author: Peter
;; Package-Requires: ((emacs "28.1") (magit "3.0") (transient "0.3"))
;; Keywords: tools, vc, git, ai
;; URL: https://github.com/11111000000/magpt

;;; Commentary:
;; Isolates Transient/Magit integration: safe transient helpers and `magpt-mode'
;; that injects entries into Magit transient menus and Magit status.
;; AI Actions use a per-repository cache keyed by Git root.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit nil t)
(require 'transient nil t)
(require 'magpt-history nil t)
(require 'magpt-log nil t)
(require 'magpt-ui-preview nil t)

;; Forward declarations (silence byte-compiler; implementations in other modules).
(declare-function magpt-commit-staged "ext:magpt")
(declare-function magpt-magit-insert-ai-overview "ext:magpt-magit-overview")
(declare-function magpt-explain-status "magpt-tasks-assist" ())
(declare-function magpt-explain-push-pull "magpt-tasks-assist" ())
(declare-function magpt-explain-branches "magpt-tasks-assist" ())
(declare-function magpt-restore-file-suggest "magpt-tasks-assist" ())
(declare-function magpt-reset-files-suggest "magpt-tasks-assist" ())
(declare-function magpt-explain-undo-commits "magpt-tasks-assist" ())
(declare-function magpt-explain-reflog-rescue "magpt-tasks-assist" ())
(declare-function magpt-explain-stash "magpt-tasks-assist" ())
(declare-function magpt-explain-detached-head "magpt-tasks-assist" ())
(declare-function magpt-explain-set-upstream "magpt-tasks-assist" ())
(declare-function magpt-ask-git "magpt-tasks-assist" ())
(declare-function magpt--eshell-popup-insert "magpt-apply" (cmd))
(declare-function magpt--i18n "ext:magpt" (key &rest args))
(declare-function magpt--history-entries-for-root "magpt-history" (root))
(declare-function magpt--history-last-entry-for "magpt-history" (task &optional root))
(declare-function magpt--entry-parse-json-safe "magpt-history" (entry))
(declare-function magpt--project-root "magpt-git")
(declare-function magpt--btn-preview-text "magpt-ui-preview" (title text &optional mode))

;; transient--prefix is defined by Transient; keep only compile-time declaration.
(eval-when-compile
  (defvar transient--prefix))

(defface magpt-transient-face
  '((t :inherit font-lock-keyword-face :foreground "green3" :weight bold))
  "Face for MaGPT entries in Magit transient menus."
  :group 'magpt)

(defun magpt--transient-desc (s)
  "Return S translated according to `magpt-info-language' (basic Russian support)."
  (let* ((lang (and (boundp 'magpt-info-language) magpt-info-language))
         (ru (and (stringp lang)
                  (string-match-p "\\`\\(ru\\|russian\\)" (downcase lang))))
         (table
          '(("AI actions (magpt)" . "Действия ИИ (magpt)")
            ("Commit with AI message (magpt)" . "Коммит с сообщением ИИ (magpt)")
            ("Suggestions" . "Подсказки")
            ("Overview/Tasks" . "Обзор/Задачи")
            ("Preview suggestion..." . "Просмотр подсказки...")
            ("Copy suggestion..." . "Копировать подсказку...")
            ("Insert first command into eshell" . "Вставить первую команду в eshell")
            ("Copy summary" . "Копировать сводку")
            ("Commit with AI message" . "Коммит с сообщением ИИ")
            ("Get new recommendations (Explain Status)" . "Получить рекомендации (Explain Status)")
            ("Push/Pull advice" . "Советы по push/pull")
            ("Explain branches" . "Объяснить ветки")
            ("Recover file..." . "Восстановить файл...")
            ("Reset files (how-to)" . "Сброс файлов (инструкции)")
            ("Undo commits (reset vs revert)" . "Отменить коммиты (reset vs revert)")
            ("Reflog rescue" . "Спасение через reflog")
            ("Stash guide" . "Руководство по stash")
            ("Detached HEAD help" . "Помощь по Detached HEAD")
            ("Set upstream help" . "Настроить upstream")
            ("Ask any question about git" . "Задать вопрос про git")
            ("Reload from overview" . "Обновить из обзора"))))
    (if ru
        (or (cdr (assoc s table)) s)
      s)))

;; Safe helpers to integrate with Transient across versions (avoid hard failures).
(defun magpt--transient-append-suffix-safe (parent pos spec)
  "Try to append SPEC after POS in PARENT transient. Return non-nil on success."
  (when (featurep 'transient)
    (condition-case e
        (prog1 t (transient-append-suffix parent pos spec))
      (error
       (when (fboundp 'magpt--log)
         (magpt--log "transient append failed: parent=%S pos=%S err=%s"
                     parent pos (magpt--errstr e)))
       nil))))

(defun magpt--transient-remove-suffix-safe (parent key)
  "Try to remove KEY from PARENT transient without throwing."
  (when (featurep 'transient)
    (ignore-errors (transient-remove-suffix parent key))))

(defun magpt--transient-add-to-magit-dispatch ()
  "Best-effort add magpt entries to `magit-dispatch' across Magit/Transient versions."
  (when (featurep 'transient)
    (let ((anchors '("!" "V" "B" "h" "t")))
      (cl-labels ((try (spec)
                    (or (seq-some (lambda (a)
                                    (magpt--transient-append-suffix-safe 'magit-dispatch a spec))
                                  anchors)
                        (magpt--transient-append-suffix-safe 'magit-dispatch nil spec))))
        (try `("." ,(magpt--transient-desc "AI actions (magpt)") magpt-ai-actions-entry))))))

;;;###autoload
(define-minor-mode magpt-mode
  "Global minor mode: integrate MaGPT with Magit’s commit transient."
  :global t
  :group 'magpt
  (if magpt-mode
      (progn
        ;; Enable: append our entries/keys/sections
        (with-eval-after-load 'magit
          ;; Commit transient: add AI commit entry
          (magpt--transient-append-suffix-safe
           'magit-commit "c"
           `("i" ,(magpt--transient-desc "Commit with AI message (magpt)") magpt-commit-staged))
          ;; Magit dispatch: robust insertion (no hard dependency on a specific anchor).
          (magpt--transient-add-to-magit-dispatch)
          ;; Direct key in Magit Status buffer: "." opens AI actions immediately (without dispatch).
          (when (boundp 'magit-status-mode-map)
            (define-key magit-status-mode-map (kbd ".") #'magpt-ai-actions-entry))
          ;; Magit Status: AI overview section (append so it appears at the bottom).
          (add-hook 'magit-status-sections-hook #'magpt-magit-insert-ai-overview t))
        ;; If Magit is already loaded, ensure immediate installation too.
        (when (featurep 'magit)
          (ignore-errors
            (magpt--transient-append-suffix-safe
             'magit-commit "c"
             `("i" ,(magpt--transient-desc "Commit with AI message (magpt)") magpt-commit-staged)))
          (ignore-errors (magpt--transient-add-to-magit-dispatch))
          (when (boundp 'magit-status-mode-map)
            (define-key magit-status-mode-map (kbd ".") #'magpt-ai-actions-entry))
          (add-hook 'magit-status-sections-hook #'magpt-magit-insert-ai-overview t)))
    ;; Disable: remove our entries/keys/sections
    (with-eval-after-load 'magit
      (magpt--transient-remove-suffix-safe 'magit-commit "i")
      (magpt--transient-remove-suffix-safe 'magit-dispatch ".")
      (when (boundp 'magit-status-mode-map)
        (define-key magit-status-mode-map (kbd ".") nil))
      (remove-hook 'magit-status-sections-hook #'magpt-magit-insert-ai-overview))
    ;; If Magit уже загружен — привести состояние к "выключено" немедленно.
    (when (featurep 'magit)
      (ignore-errors (magpt--transient-remove-suffix-safe 'magit-commit "i"))
      (ignore-errors (magpt--transient-remove-suffix-safe 'magit-dispatch "."))
      (when (boundp 'magit-status-mode-map)
        (define-key magit-status-mode-map (kbd ".") nil))
      (remove-hook 'magit-status-sections-hook #'magpt-magit-insert-ai-overview))))

;; -----------------------------------------------------------------------------
;; AI Actions — per-root cache and transient UI
;; -----------------------------------------------------------------------------

(defvar magpt--ai-actions-state (make-hash-table :test 'equal)
  "Map root → plist (:summary STRING :suggestions LIST).
Per-repository cache for AI Actions transient.")

(defcustom magpt-ai-actions-source-tasks
  '(explain-status
    ask-git
    explain-push-pull
    explain-branches
    restore-file-suggest
    reset-files-suggest
    explain-undo-commits
    explain-reflog-rescue
    explain-stash
    explain-detached-head
    explain-set-upstream)
  "Tasks to source suggestions from for AI Actions.
The newest valid entry among these tasks supplies summary and suggestions."
  :type '(repeat (choice (const explain-status)
                         (const ask-git)
                         (const explain-push-pull)
                         (const explain-branches)
                         (const restore-file-suggest)
                         (const reset-files-suggest)
                         (const explain-undo-commits)
                         (const explain-reflog-rescue)
                         (const explain-stash)
                         (const explain-detached-head)
                         (const explain-set-upstream)))
  :group 'magpt)

(defun magpt--ai--normalize-suggestions (data)
  "Return normalized suggestions plist list from parsed DATA alist."
  (let ((sugs (and (listp data) (alist-get 'suggestions data))))
    (when (listp sugs)
      (mapcar
       (lambda (s)
         (let* ((title (or (alist-get 'title s) ""))
                (cmds  (let ((cs (alist-get 'commands s)))
                         (if (listp cs)
                             (mapconcat (lambda (c) (format "%s" c)) cs "\n")
                           "")))
                (keys  (let ((ks (or (alist-get 'keys s)
                                     (alist-get 'magit_keys s))))
                         (and (listp ks) (seq-filter #'stringp ks)))))
           (list :title title :commands cmds :keys keys)))
       sugs))))

(defun magpt--ai--latest-entry-for-any (tasks)
  "Return latest entry whose :task is a member of TASKS for the current repo."
  (let* ((root (ignore-errors (magpt--project-root)))
         (lst (and root (magpt--history-entries-for-root root)))
         (set (and (listp tasks) tasks)))
    (seq-find (lambda (e) (memq (plist-get e :task) set)) lst)))

(defun magpt--ai-suggestions-and-summary ()
  "Return cons (SUGGESTIONS . SUMMARY) from newest of source tasks for current repo."
  (let* ((e (magpt--ai--latest-entry-for-any magpt-ai-actions-source-tasks))
         (data (and e (magpt--entry-parse-json-safe e)))
         (summary (and data (alist-get 'summary data)))
         (sugs (and data (magpt--ai--normalize-suggestions data))))
    (cons (or sugs '()) (and (stringp summary) summary))))

(defun magpt--ai-actions-init ()
  "Initialize AI actions state from history for the current repo.
Return number of suggestions."
  (pcase-let* ((`(,sugs . ,summary) (magpt--ai-suggestions-and-summary)))
    (let ((root (ignore-errors (magpt--project-root))))
      (when root
        (puthash root (list :summary (and (stringp summary) summary)
                            :suggestions sugs)
                 magpt--ai-actions-state)))
    (let ((n (length sugs)))
      (when (fboundp 'magpt--log)
        (magpt--log "ai-actions-init: suggestions=%d summary?=%s"
                    n (if (and (stringp summary) (> (length summary) 0)) "t" "nil")))
      n)))

(defun magpt--ai-actions-choose-index ()
  "Prompt for a suggestion index using completing-read."
  (let* ((root (ignore-errors (magpt--project-root))))
    (unless (and root (plist-get (gethash root magpt--ai-actions-state) :suggestions))
      (magpt--ai-actions-init))
    (let* ((state (and root (gethash root magpt--ai-actions-state)))
           (sugs (or (plist-get state :suggestions) '()))
           (titles (mapcar (lambda (it) (plist-get it :title)) sugs))
           (choice (completing-read (magpt--i18n 'ai-suggest-prompt) titles nil t)))
      (cl-position choice titles :test #'string=))))

;;;###autoload
(defun magpt-ai-actions-preview (&optional idx)
  "Preview commands for a suggestion (open read-only buffer with shell-mode)."
  (interactive)
  (let* ((root (ignore-errors (magpt--project-root))))
    (unless (and root (plist-get (gethash root magpt--ai-actions-state) :suggestions))
      (magpt--ai-actions-init))
    (let* ((state (and root (gethash root magpt--ai-actions-state)))
           (sugs (or (plist-get state :suggestions) '())))
      (if (zerop (length sugs))
          (user-error "%s" (magpt--i18n 'ai-no-suggestions))
        (let* ((i (or idx (magpt--ai-actions-choose-index)))
               (sug (nth i sugs))
               (title (plist-get sug :title))
               (cmds (plist-get sug :commands))
               (keys (plist-get sug :keys))
               (keys-str (and (listp keys) (string-join (mapcar (lambda (k) (format "%s" k)) keys) ", ")))
               (body (if keys-str
                         (format "# Magit keys: %s\n\n%s" keys-str cmds)
                       cmds)))
          (when (fboundp 'magpt--log)
            (magpt--log "ai-actions-preview: idx=%s title=%s keys=%s"
                        i title (or keys-str "[]")))
          (magpt--btn-preview-text (format "AI suggestion: %s" title) body 'shell))))))

;;;###autoload
(defun magpt-ai-actions-copy (&optional idx)
  "Copy commands for a suggestion to the kill-ring."
  (interactive)
  (let* ((root (ignore-errors (magpt--project-root))))
    (unless (and root (plist-get (gethash root magpt--ai-actions-state) :suggestions))
      (magpt--ai-actions-init))
    (let* ((state (and root (gethash root magpt--ai-actions-state)))
           (sugs (or (plist-get state :suggestions) '())))
      (if (zerop (length sugs))
          (user-error "%s" (magpt--i18n 'ai-no-suggestions))
        (let* ((i (or idx (magpt--ai-actions-choose-index)))
               (sug (nth i sugs))
               (cmds (plist-get sug :commands)))
          (kill-new cmds)
          (message "%s" (magpt--i18n 'ai-suggest-copied)))))))

;;;###autoload
(defun magpt-ai-actions-eshell-insert (&optional idx)
  "Insert the first command of a suggestion into an eshell popup."
  (interactive)
  (let* ((root (ignore-errors (magpt--project-root))))
    (unless (and root (plist-get (gethash root magpt--ai-actions-state) :suggestions))
      (magpt--ai-actions-init))
    (let* ((state (and root (gethash root magpt--ai-actions-state)))
           (sugs (or (plist-get state :suggestions) '())))
      (if (zerop (length sugs))
          (user-error "%s" (magpt--i18n 'ai-no-suggestions))
        (let* ((i (or idx (magpt--ai-actions-choose-index)))
               (sug (nth i sugs))
               (cmds (plist-get sug :commands))
               (lines (and (stringp cmds) (split-string cmds "\n")))
               (first (and lines (seq-find (lambda (l)
                                             (and (stringp l)
                                                  (> (length (string-trim l)) 0)
                                                  (not (string-prefix-p "#" (string-trim-left l))))))
                           lines))))
        (unless (stringp first)
          (user-error "%s" (magpt--i18n 'ai-no-shell-cmd)))
        (unless (fboundp 'magpt--eshell-popup-insert)
          (require 'magpt-apply nil t))
        (if (fboundp 'magpt--eshell-popup-insert)
            (magpt--eshell-popup-insert (string-trim first))
          (user-error "%s" (magpt--i18n 'ai-eshell-helper-missing)))))))

;;;###autoload
(defun magpt-ai-actions-copy-summary ()
  "Copy the latest summary to the kill-ring."
  (interactive)
  (let* ((root (ignore-errors (magpt--project-root)))
         (state (and root (gethash root magpt--ai-actions-state)))
         (summary (and state (plist-get state :summary))))
    (unless (and (stringp summary) (> (length (string-trim summary)) 0))
      (magpt--ai-actions-init)
      (setq state (and root (gethash root magpt--ai-actions-state)))
      (setq summary (and state (plist-get state :summary))))
    (if (not (and (stringp summary) (> (length (string-trim summary)) 0)))
        (user-error "%s" (magpt--i18n 'ai-no-summary))
      (kill-new summary)
      (message "%s" (magpt--i18n 'ai-summary-copied)))))

;;;###autoload
(defun magpt-ai-actions-reload ()
  "Reload AI actions state from the overview and refresh transient UI."
  (interactive)
  (let ((n (magpt--ai-actions-init)))
    (when (fboundp 'magpt--log)
      (magpt--log "ai-actions-reload: suggestions=%d" n)))
  (when (featurep 'transient)
    (condition-case e
        (progn
          (when (fboundp 'magpt--log)
            (magpt--log "ai-actions-reload: calling magpt-ai-actions interactively"))
          (call-interactively #'magpt-ai-actions)
          (when (fboundp 'magpt--log)
            (magpt--log "ai-actions-reload: magpt-ai-actions OK")))
      (error
       (when (fboundp 'magpt--log)
         (magpt--log "ai-actions-reload: interactive magpt-ai-actions error: %s"
                     (magpt--errstr e)))))))

(defcustom magpt-ai-actions-auto-reload t
  "If non-nil, automatically refresh AI Actions suggestions when history changes.
When the AI Actions transient is open, the UI is also reloaded."
  :type 'boolean
  :group 'magpt)

(defun magpt--ai-actions-history-updated (&optional root)
  "Hook: called when AI history changes. Refresh AI Actions state/UI for this ROOT only."
  (when magpt-ai-actions-auto-reload
    (let* ((curr (ignore-errors (magpt--project-root)))
           (curr-norm (and curr (file-name-as-directory (expand-file-name curr))))
           (arg-norm  (and root (file-name-as-directory (expand-file-name root)))))
      (when (or (null root) (and curr-norm arg-norm (string= curr-norm arg-norm)))
        (when (fboundp 'magpt--log)
          (magpt--log "ai-actions: history-changed for root=%s; refreshing suggestions cache" arg-norm))
        ;; Refresh internal cache for the current repo
        (ignore-errors (magpt--ai-actions-init))
        ;; If AI Actions transient is currently open, reload UI
        (when (and (featurep 'transient)
                   (boundp 'transient--prefix) transient--prefix
                   (ignore-errors (eq (oref transient--prefix command) 'magpt-ai-actions)))
          (when (fboundp 'magpt--log)
            (magpt--log "ai-actions: transient open → reloading UI"))
          (ignore-errors (magpt-ai-actions-reload)))))))

(add-hook 'magpt-history-changed-hook #'magpt--ai-actions-history-updated)

(defun magpt--ai--short-root ()
  "Return short repo name for current root."
  (let ((root (ignore-errors (magpt--project-root))))
    (and root (file-name-nondirectory (directory-file-name root)))))

(with-eval-after-load 'transient
  (transient-define-prefix magpt-ai-actions ()
    "AI actions (magpt)"
    [["Suggestions"
      ("p" magpt-ai-actions-preview
       :description (lambda () (magpt--transient-desc "Preview suggestion...")))
      ("y" magpt-ai-actions-copy
       :description (lambda () (magpt--transient-desc "Copy suggestion...")))
      ("e" magpt-ai-actions-eshell-insert
       :description (lambda () (magpt--transient-desc "Insert first command into eshell")))
      ("s" magpt-ai-actions-copy-summary
       :description (lambda () (magpt--transient-desc "Copy summary")))
      ("c" magpt-commit-staged
       :description (lambda () (magpt--transient-desc "Commit with AI message")))]
     ["Overview/Tasks"
      ("g" magpt-explain-status
       :description (lambda () (magpt--transient-desc "Get new recommendations (Explain Status)")))
      ("u" magpt-explain-push-pull
       :description (lambda () (magpt--transient-desc "Push/Pull advice")))
      ("b" magpt-explain-branches
       :description (lambda () (magpt--transient-desc "Explain branches")))
      ("f" magpt-restore-file-suggest
       :description (lambda () (magpt--transient-desc "Recover file...")))
      ("x" magpt-reset-files-suggest
       :description (lambda () (magpt--transient-desc "Reset files (how-to)")))
      ("o" magpt-explain-undo-commits
       :description (lambda () (magpt--transient-desc "Undo commits (reset vs revert)")))
      ("L" magpt-explain-reflog-rescue
       :description (lambda () (magpt--transient-desc "Reflog rescue")))
      ("t" magpt-explain-stash
       :description (lambda () (magpt--transient-desc "Stash guide")))
      ("D" magpt-explain-detached-head
       :description (lambda () (magpt--transient-desc "Detached HEAD help")))
      ("S" magpt-explain-set-upstream
       :description (lambda () (magpt--transient-desc "Set upstream help")))
      ("?" magpt-ask-git
       :description (lambda () (magpt--transient-desc "Ask any question about git")) )
      ("r" magpt-ai-actions-reload
       :description (lambda () (magpt--transient-desc "Reload from overview")))]]))

(unless (fboundp 'magpt-ai-actions)
  (defun magpt-ai-actions ()
    "Fallback AI actions when `transient' is not available."
    (interactive)
    (let ((n (magpt--ai-actions-init)))
      (when (fboundp 'magpt--log)
        (magpt--log "ai-actions(fallback): init suggestions=%d" n))
      (condition-case e
          (call-interactively #'magpt-ai-actions-preview)
        (error
         (when (fboundp 'magpt--log)
           (magpt--log "ai-actions(fallback): preview error: %s" (magpt--errstr e)))
         (message "%s" (magpt--i18n 'callback-error (magpt--errstr e))))))))

;;;###autoload
(defun magpt-ai-actions-entry ()
  "Entry point for '.' key in Magit; logs and opens AI actions."
  (interactive)
  (when (fboundp 'magpt--log)
    (magpt--log "key [.]: magpt-ai-actions-entry buffer=%s root=%s transient?=%s"
                (buffer-name)
                (ignore-errors (magpt--project-root))
                (if (featurep 'transient) "t" "nil")))
  (if (featurep 'transient)
      (condition-case e
          (progn
            (when (fboundp 'magpt--log)
              (magpt--log "ai-actions-entry: calling magpt-ai-actions interactively"))
            (call-interactively #'magpt-ai-actions)
            (when (fboundp 'magpt--log)
              (magpt--log "ai-actions-entry: interactive magpt-ai-actions OK")))
        (error
         (when (fboundp 'magpt--log)
           (magpt--log "ai-actions-entry: interactive magpt-ai-actions error: %s; fallback to text UI"
                       (magpt--errstr e)))
         (magpt-ai-actions)))
    (progn
      (when (fboundp 'magpt--log)
        (magpt--log "ai-actions-entry: transient not present; using fallback UI"))
      (magpt-ai-actions))))

(provide 'magpt-transient)

;;; magpt-transient.el ends here
