;;; magpt-tasks-assist.el --- Assist (Phase 1) tasks for MaGPT  -*- lexical-binding: t; -*-

;; Author: Peter
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, vc, git, ai
;; URL: https://github.com/11111000000/magpt

;;; Commentary:
;; Phase 1 assist tasks (Explain Status, Commit Lint, Branch Name Suggest).
;; These are the real implementations (context → prompt → render) extracted
;; from magpt.el so they live in their own module.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit nil t)

;; Forward declarations to other magpt modules / core helpers.
(declare-function magpt--history-append-entry "magpt-history" (task request response &optional note &rest kvs))
(declare-function magpt--history-last-entry-for "magpt-history" (task))
(declare-function magpt--entry-parse-json-safe "magpt-history" (entry))
(declare-function magpt--log "ext:magpt" (fmt &rest args))
(declare-function magpt--backtrace-string "ext:magpt")
(declare-function magpt--project-root "magpt-git")
(declare-function magpt--git "magpt-git" (dir &rest args))
(declare-function magpt--recent-git-output-get "magpt-git" (dir))
(declare-function magpt--string-bytes "magpt-git" (s))
(declare-function magpt-register-task "ext:magpt" (task))
(declare-function magpt--task "ext:magpt" (&rest args))
(declare-function magpt--hash-table-keys "ext:magpt" (ht))
(declare-function magpt-run-task "ext:magpt" (name &optional ctx))
(declare-function magpt--entry-ensure-json "ext:magpt" (entry))
(declare-function magpt--i18n "ext:magpt" (key &rest args))
(declare-function magpt--maybe-load-rc "ext:magpt")

(defcustom magpt-include-magit-keys-in-suggestions t
  "If non-nil, include a dynamic Magit key cheatsheet in Explain Status prompts.
The model is instructed to add a `keys' array to each suggestion with matching key
sequences taken ONLY from the cheatsheet. If no suitable key exists, it should
use an empty list instead of guessing."
  :type 'boolean
  :group 'magpt)

(defcustom magpt-educational-mode t
  "If non-nil, allow prompts to request optional teaching fields (rationale, steps).
Models may include these fields in suggestions[]. They are ignored by the UI unless shown explicitly."
  :type 'boolean
  :group 'magpt)

(defun magpt--magit-keys-runtime ()
  "Return an alist of (DESCRIPTION . (KEYS...)) for common Magit actions.
Keys are resolved from `magit-status-mode-map' and Transient suffixes when available."
  (when (and (featurep 'magit) (boundp 'magit-status-mode-map))
    (let* ((map magit-status-mode-map))
      (cl-labels
          ((keys-for (fn)
             (when (fboundp fn)
               (mapcar #'key-description (where-is-internal fn map))))
           (prefix-keys-for (prefix-fn)
             (when (fboundp prefix-fn)
               (mapcar #'key-description (where-is-internal prefix-fn map))))
           (clean-keys (keys)
             (delete-dups
              (seq-filter (lambda (k)
                            (and (stringp k)
                                 (not (string-match-p "<menu-bar" k))))
                          (seq-filter #'identity keys))))
           (composite-keys (prefix-fn target-fn)
             (when (and (featurep 'transient) (fboundp 'transient-get-suffixes))
               (let* ((pkeys (prefix-keys-for prefix-fn))
                      (suffixes (ignore-errors (transient-get-suffixes prefix-fn))))
                 (when (and pkeys suffixes)
                   (cl-loop for suf in suffixes
                            for cmd = (ignore-errors (oref suf command))
                            for skey = (ignore-errors (oref suf key))
                            when (and (eq cmd target-fn)
                                      (stringp skey) (> (length skey) 0))
                            append (mapcar (lambda (pk) (format "%s %s" pk skey)) pkeys)))))))
        (let* ((pairs
                (list
                 (cons "Stage selection"         (keys-for 'magit-stage))
                 (cons "Unstage selection"       (keys-for 'magit-unstage))
                 (cons "Stage all"               (keys-for 'magit-stage-modified))
                 (cons "Unstage all"             (keys-for 'magit-unstage-all))
                 (cons "Commit (create)"
                       (or (keys-for 'magit-commit-create)
                           (composite-keys 'magit-commit 'magit-commit-create)
                           '("c c")))  ;; fallback
                 (cons "Commit (amend)"
                       (or (keys-for 'magit-commit-amend)
                           (composite-keys 'magit-commit 'magit-commit-amend)
                           '("c a")))  ;; fallback
                 (cons "Commit with AI message (magpt)"
                       (or (keys-for 'magpt-commit-staged)
                           (composite-keys 'magit-commit 'magpt-commit-staged)
                           '("c i")))  ;; fallback
                 (cons "Push (to push-remote)"
                       (or (keys-for 'magit-push-current)
                           (keys-for 'magit-push)
                           (composite-keys 'magit-push 'magit-push-current)))
                 (cons "Fetch"
                       (or (keys-for 'magit-fetch)
                           (composite-keys 'magit-fetch 'magit-fetch)))
                 (cons "Pull"
                       (or (keys-for 'magit-pull)
                           (composite-keys 'magit-pull 'magit-pull)))
                 (cons "Switch branch"
                       (or (keys-for 'magit-branch-checkout)
                           (keys-for 'magit-checkout)
                           (composite-keys 'magit-branch 'magit-branch-checkout)))
                 (cons "Create branch"
                       (or (keys-for 'magit-branch-and-checkout)
                           (keys-for 'magit-branch-create)
                           (composite-keys 'magit-branch 'magit-branch-and-checkout)
                           (composite-keys 'magit-branch 'magit-branch-create)))
                 (cons "Log (current)"
                       (or (keys-for 'magit-log-current)
                           (composite-keys 'magit-log 'magit-log-current)))
                 (cons "Diff (working tree)"
                       (or (keys-for 'magit-diff-working-tree)
                           (keys-for 'magit-diff)
                           (composite-keys 'magit-diff 'magit-diff-working-tree)))
                 (cons "Stash"
                       (or (keys-for 'magit-stash)
                           (composite-keys 'magit-stash 'magit-stash)))
                 (cons "Blame"                   (keys-for 'magit-blame))
                 (cons "Rebase"
                       (or (keys-for 'magit-rebase)
                           (composite-keys 'magit-rebase 'magit-rebase)))))
               out)
          (dolist (p pairs)
            (let* ((desc (car p))
                   (ks (clean-keys (cdr p))))
              (when ks
                (push (cons desc ks) out))))
          (nreverse out))))))

(defun magpt--format-magit-keys-cheatsheet ()
  "Format runtime Magit key bindings as a simple cheatsheet string."
  (let ((alist (magpt--magit-keys-runtime)))
    (if (null alist)
        ""
      (mapconcat
       (lambda (pair)
         (format "- %s: %s" (car pair) (mapconcat #'identity (cdr pair) ", ")))
       alist "\n"))))

(defun magpt--format-magit-keys-cheatsheet-safe ()
  "Safe wrapper around `magpt--format-magit-keys-cheatsheet' with timeout and logging."
  (let ((start (float-time)) out)
    (condition-case err
        (with-timeout (0.5 (setq out ""))
          (setq out (or (magpt--format-magit-keys-cheatsheet) "")))
      (error
       (magpt--log "magit-keys-cheatsheet error: %s" (error-message-string err))
       (setq out "")))
    (magpt--log "magit-keys-cheatsheet: dur=%.3fs bytes=%d"
                (- (float-time) start) (length out))
    out))

(defun magpt--porcelain-parse (porc)
  "Parse 'git status --porcelain' into staged/unstaged/untracked file lists."
  (let (staged unstaged untracked)
    (dolist (l (split-string (or porc "") "\n" t))
      (when (string-match "\\`\\([ MADRCU?]\\)\\([ MADRCU?]\\) \\(.*\\)\\'" l)
        (let ((x (match-string 1 l))
              (y (match-string 2 l))
              (path (match-string 3 l)))
          (cond
           ;; Untracked
           ((and (string= x "?") (string= y "?"))
            (push path untracked))
           (t
            ;; Any non-space X means there is something staged for this path.
            (when (not (string= x " "))
              (push path staged))
            ;; Any non-space Y means there is something unstaged in worktree.
            (when (not (string= y " "))
              (push path unstaged)))))))
    (list :staged (nreverse staged)
          :unstaged (nreverse unstaged)
          :untracked (nreverse untracked))))

(defun magpt--ctx-status (_ctx)
  "Collect minimal git status for explain-status task, recent git output, plus Magit keys (optional).
Return (data preview bytes). DATA is a plist with :status, :recent-git-output, optional :magit-keys,
and parsed lists :staged-files, :unstaged-files, :untracked-files."
  (let* ((root (magpt--project-root)))
    (magpt--log "ctx-status: root=%s begin" root)
    ;; Raw porcelain for XY legend (for precise index/worktree interpretation).
    (magpt--log "ctx-status: calling: git status --porcelain")
    (let* ((porc (magpt--git root "status" "--porcelain"))
           (short (string-join (seq-take (split-string porc "\n" t) 200) "\n"))
           (parsed (magpt--porcelain-parse porc))
           (staged   (plist-get parsed :staged))
           (unstaged (plist-get parsed :unstaged))
           (untracked (plist-get parsed :untracked)))
      (magpt--log "ctx-status: porcelain lines=%d" (length (split-string short "\n" t)))
      ;; Additionally update per-repo log: short human-readable status with branches.
      (magpt--log "ctx-status: calling: git status -sb")
      (let* ((_ignore (ignore-errors (magpt--git root "status" "-sb"))))
        (magpt--log "ctx-status: recent-get begin")
        (let* ((recent (magpt--recent-git-output-get root)))
          (magpt--log "ctx-status: recent-get done chars=%d" (length (or recent "")))
          (let* ((t0 (float-time))
                 (lst (split-string (or recent "") "\n" t))
                 (len (length lst))
                 (sz 42)
                 (recent-trunc (string-join (last lst sz) "\n")))
            (magpt--log "ctx-status: recent-trunc done lines=%d kept=%d dur=%.3fs"
                        len (min len sz) (- (float-time) t0))
            (let* ((keys (when magpt-include-magit-keys-in-suggestions
                           (progn
                             (magpt--log "ctx-status: computing Magit keys cheatsheet...")
                             (magpt--format-magit-keys-cheatsheet-safe))))
                   (keys-bytes (length (or keys ""))))
              (when magpt-include-magit-keys-in-suggestions
                (magpt--log "ctx-status: keys computed bytes=%d" keys-bytes))
              (let* ((preview (concat
                               (if (and keys (not (string-empty-p keys)))
                                   (format "STATUS:\n%s\n\nMAGIT KEYS:\n%s"
                                           short
                                           (string-join (seq-take (split-string keys "\n" t) 50) "\n"))
                                 short)
                               (when (> (length recent-trunc) 0)
                                 (concat "\n\nRECENT GIT OUTPUT:\n" recent-trunc))))
                     (bytes (magpt--string-bytes preview)))
                (magpt--log "ctx-status: end root=%s status-lines=%d recent-lines=%d keys?=%s preview-bytes=%d"
                            root
                            (length (split-string short "\n" t))
                            (length (split-string recent-trunc "\n" t))
                            (if (and keys (not (string-empty-p keys))) "t" "nil")
                            bytes)
                (list (list :status short
                            :magit-keys keys
                            :recent-git-output recent-trunc
                            :staged-files staged
                            :unstaged-files unstaged
                            :untracked-files untracked)
                      preview bytes)))))))))

(defun magpt--prompt-explain-status (status-or-data)
  "Build prompt for explain-status. Use `magpt-info-language' for textual fields.
Be sure to consider all messages, errors, hints and warnings present in the 'RECENT GIT OUTPUT' section below."
  (let* ((ilang (or magpt-info-language "English"))
         (status (if (stringp status-or-data)
                     status-or-data
                   (or (plist-get status-or-data :status) "")))
         (recent-output (and (not (stringp status-or-data))
                             (plist-get status-or-data :recent-git-output)))
         (keys   (and (not (stringp status-or-data))
                      (plist-get status-or-data :magit-keys)))
         (staged-files   (and (not (stringp status-or-data))
                              (plist-get status-or-data :staged-files)))
         (unstaged-files (and (not (stringp status-or-data)
                                   ) (plist-get status-or-data :unstaged-files)))
         (untracked-files (and (not (stringp status-or-data))
                               (plist-get status-or-data :untracked-files)))
         (have-keys (and keys (stringp keys) (> (length keys) 0)))
         (keys-block
          (if have-keys
              (concat
               "\nUse ONLY the Magit key bindings listed below for suggestions[].keys.\n"
               "If no suitable action exists, use [].\n"
               "--- BEGIN MAGIT KEYS HELP ---\n" keys "\n--- END MAGIT KEYS HELP ---\n")
            "\nIf relevant Magit key bindings are known, include them in suggestions[].keys; otherwise use [].\n"))
         (facts-block
          (let* ((sf (and (listp staged-files) staged-files))
                 (uf (and (listp unstaged-files) unstaged-files)
                     )
                 (uf2 (and (listp untracked-files) untracked-files)))
            (format (concat
                     "Authoritative facts derived from porcelain (do NOT contradict these):\n"
                     "- If a file is in STAGED FILES and NOT in UNSTAGED FILES, it MUST NOT be called 'unstaged', and you MUST NOT suggest 'git add' for it.\n"
                     "- Only files listed in UNSTAGED FILES may need 'git add'.\n\n"
                     "--- BEGIN FACTS ---\n"
                     "STAGED FILES (%d):\n%s\n\n"
                     "UNSTAGED FILES (%d):\n%s\n\n"
                     "UNTRACKED FILES (%d):\n%s\n"
                     "--- END FACTS ---\n")
                    (length sf) (if (and sf (> (length sf) 0)) (string-join sf "\n") "(none)")
                    (length uf) (if (and uf (> (length uf) 0)) (string-join uf "\n") "(none)")
                    (length uf2) (if (and uf2 (> (length uf2) 0)) (string-join uf2 "\n") "(none)"))))
         (recent-block
          (if (and (stringp recent-output) (> (length recent-output) 0))
              (format "\n--- RECENT GIT OUTPUT ---\n%s\n--- END RECENT GIT OUTPUT ---\n" recent-output)
            "")))
    (format (concat
             "You are an experienced developer. Analyze the current repository status and the full output of recent Git commands.\n"
             "Make sure to account for all messages, errors, hints, and warnings present in the RECENT GIT OUTPUT section below.\n"
             "If it suggests actions, shows conflicts, or requires the user to finish an interactive step — tailor your recommendations to that specifically.\n"
             "Answer STRICTLY in %s. Return ONLY JSON with fields:\n"
             "  summary: string,\n"
             "  risks:   array of strings,\n"
             "  suggestions: array of {\n"
             "    title: string,\n"
             "    commands: array of strings,\n"
             "    keys: array of strings  // Magit key sequences to recommend; if unknown — []\n"
             "  }\n"
             "You MAY include optional fields in suggestions items: rationale (string), steps (array of strings).\n"
             "Instructions:\n"
             "- Interpret Git status codes (see legend), but if there is recent command output with errors/hints/pending steps — PRIORITIZE analyzing that output."
             " Provide solutions and warnings based on the actual messages.\n"
             "- For example: editor needs to be closed, pull conflicts, choosing rebase/merge strategy, etc.\n"
             "- Never suggest stage/unstage for changes already staged.\n"
             "- Use the FACTS block below as authoritative truth when deciding whether a file is staged or unstaged.\n"
             "- For a commit with an AI-generated message prefer [c i] if available; otherwise [c c].\n"
             "%s%s"
             "\n--- BEGIN STATUS ---\n%s\n--- END STATUS ---\n"
             "%s"
             "\n--- GIT PORCELAIN LEGEND ---\n"
             "XY columns (X=index/staged, Y=worktree):\n"
             "  M  file   → staged modify (index changed)\n"
             "   M file   → unstaged modify (worktree changed)\n"
             "  A  file   → staged add\n"
             "  D  file   → staged delete\n"
             "   D file   → unstaged delete\n"
             "  R  old -> new → staged rename\n"
             "  C  old -> new → staged copy\n"
             "  U? or ?U or UU → merge conflict\n"
             "  ?? file   → untracked file\n"
             "--- END GIT PORCELAIN LEGEND ---\n")
            ilang keys-block facts-block status recent-block)))

(defun magpt--render-explain-status (json data)
  (magpt--history-append-entry 'explain-status (or magpt--current-request "") (or json "")
                               "JSON schema: {summary, risks[], suggestions[].commands[], suggestions[].keys[]}"
                               :status-snapshot (or (and (listp data) (plist-get data :status)) "")))

;; Commit Lint/Fix Suggest

(defun magpt--ctx-commit-lint (_ctx)
  "Collect current commit message (top section only) and staged diff.
Return (data preview bytes)."
  (let* ((buf (or (and (magpt--commit-buffer-p) (current-buffer))
                  (magpt--find-commit-buffer))))
    (unless buf
      (user-error "No commit buffer found"))
    (with-current-buffer buf
      (let* ((bounds (magpt--commit-message-boundaries))
             (msg-end (car bounds))
             (msg (string-trim (buffer-substring-no-properties (point-min) msg-end))))
        (when (string-empty-p msg)
          (user-error "Commit message is empty; write something to lint or generate a message first"))
        (let* ((root (magpt--project-root))
               (diff (magpt--staged-diff root))
               (preview (format "MSG:\n%s\n\n--- DIFF (truncated for preview) ---\n%s"
                                msg
                                (if (> (length diff) 2000) (concat (substring diff 0 2000) " …") diff)))
               (bytes (+ (magpt--string-bytes msg) (magpt--string-bytes diff))))
          (list (list :message msg :diff diff) preview bytes))))))

(defun magpt--prompt-commit-lint (data)
  "Build prompt for commit lint task using DATA (:message :diff).
Uses `magpt-commit-language' for suggestion.message and `magpt-info-language' for issues[]."
  (let* ((msg (plist-get data :message))
         (diff (plist-get data :diff))
         (lang-lines
          (concat
           (when (and (stringp magpt-commit-language)
                      (> (length magpt-commit-language) 0))
             (format "Write suggestion.message STRICTLY in %s.\n" magpt-commit-language))
           (when (and (stringp magpt-info-language)
                      (> (length magpt-info-language) 0))
             (format "Use %s for any explanatory text (e.g., issues[] strings). Answer strictly in this language for non-JSON fields.\n" magpt-info-language)))))
    (format (concat
             "You lint Git commit messages against Conventional Commits.\n"
             "Given the current commit message and staged diff, identify issues and suggest a fix.\n"
             "Rules:\n"
             "- Keep title <= 72 chars, imperative mood.\n"
             "- Suggest minimal changes; don't invent ticket IDs.\n"
             "Return ONLY JSON with fields:\n"
             "  status: \"ok\" | \"issues\",\n"
             "  issues: array of strings,\n"
             "  suggestion: { replace: boolean, message: string }\n\n"
             "%s"
             "--- BEGIN MESSAGE ---\n%s\n--- END MESSAGE ---\n\n"
             "--- BEGIN DIFF ---\n%s\n--- END DIFF ---\n")
            lang-lines
            msg
            diff)))

(defun magpt--render-commit-lint (json data)
  (ignore data)
  (magpt--history-append-entry 'commit-lint-suggest (or magpt--current-request "") (or json "")
                               "JSON schema: {status, issues[], suggestion{replace,message}}"))

;; Branch Name Suggest

(defun magpt--ctx-branch-name (_ctx)
  "Collect brief context for branch name suggestion: porcelain status paths."
  (let* ((root (magpt--project-root))
         (porc (magpt--git root "status" "--porcelain"))
         (paths (mapcar (lambda (l) (string-trim (substring l 3)))
                        (seq-filter (lambda (l) (>= (length l) 3))
                                    (split-string porc "\n" t))))
         (preview (string-join (seq-take paths 50) "\n"))
         (bytes (magpt--string-bytes preview)))
    (list (list :paths paths) preview bytes)))

(defun magpt--prompt-branch-name (data)
  "Build prompt for branch-name-suggest task using DATA (:paths)."
  (let ((paths (plist-get data :paths))
        (ilang (or magpt-info-language "English")))
    (format (concat
             "Propose a safe Git branch name in kebab-case for the current work.\n"
             "Constraints:\n"
             "- lowercase, kebab-case, <= 40 chars, [a-z0-9-] only, no trailing dash.\n"
             "- Prefer intent over specifics; no secrets.\n"
             "Use %s for textual fields like rationale. Answer strictly in this language.\n"
             "Return ONLY JSON with fields:\n"
             "  name: string,\n"
             "  alternatives: array of strings,\n"
             "  rationale: string\n\n"
             "--- BEGIN PATHS ---\n%s\n--- END PATHS ---\n")
            ilang
            (string-join paths "\n"))))

(defun magpt--render-branch-name (json _data)
  (magpt--history-append-entry 'branch-name-suggest (or magpt--current-request "") (or json "")
                               "JSON schema: {name, alternatives[], rationale}"))

;; Push/Pull advice

(defun magpt--ctx-push-pull (_ctx)
  "Collect context for push/pull advice: status -sb, upstream ahead/behind, remotes, recent output."
  (let* ((root (magpt--project-root))
         (status (ignore-errors (magpt--git root "status" "-sb")))
         (remote (ignore-errors (magpt--git root "remote" "-v")))
         (upstream (ignore-errors (magpt--git root "rev-parse" "--abbrev-ref" "@{upstream}")))
         (ahead nil) (behind nil))
    (when upstream
      (let* ((lr (ignore-errors (magpt--git root "rev-list" "--left-right" "--count" "@{upstream}...HEAD")))
             (parts (and lr (split-string lr "[ \t]+" t))))
        (when (and parts (= (length parts) 2))
          (setq behind (nth 0 parts))
          (setq ahead  (nth 1 parts)))))
    (let* ((recent (magpt--recent-git-output-get root))
           (recent-trunc (string-join (last (split-string (or recent "") "\n" t) 42) "\n"))
           (keys (when magpt-include-magit-keys-in-suggestions
                   (magpt--format-magit-keys-cheatsheet-safe)))
           (preview (string-join
                     (delq nil
                           (list
                            (and status (format "STATUS -sb:\n%s" status))
                            (and upstream (format "\nUPSTREAM: %s (ahead %s, behind %s)"
                                                  upstream (or ahead "0") (or behind "0")))
                            (and remote (format "\n\nREMOTES:\n%s" remote))
                            (and (and keys (> (length keys) 0))
                                 (format "\n\nMAGIT KEYS:\n%s"
                                         (string-join (seq-take (split-string keys "\n" t) 50) "\n")))
                            (and (> (length recent-trunc) 0)
                                 (format "\n\nRECENT GIT OUTPUT:\n%s" recent-trunc))))
                     ""))
           (bytes (magpt--string-bytes preview)))
      (list (list :status status :remote remote :upstream upstream
                  :ahead ahead :behind behind :recent recent-trunc :magit-keys keys)
            preview bytes))))

(defun magpt--prompt-explain-push-pull (data)
  "Build prompt to advise on push/pull given DATA."
  (let* ((ilang (or magpt-info-language "English"))
         (status (or (plist-get data :status) ""))
         (up (or (plist-get data :upstream) "(none)"))
         (ahead (or (plist-get data :ahead) "0"))
         (behind (or (plist-get data :behind) "0"))
         (rem (or (plist-get data :remote) ""))
         (recent (or (plist-get data :recent) ""))
         (keys (plist-get data :magit-keys))
         (keys-block
          (if (and keys (stringp keys) (> (length keys) 0))
              (format "\nUse ONLY the Magit key bindings listed below for suggestions[].keys; otherwise use [].\n--- BEGIN MAGIT KEYS HELP ---\n%s\n--- END MAGIT KEYS HELP ---\n" keys)
            "\nIf relevant Magit key bindings are known, include them in suggestions[].keys; otherwise use [].\n")))
    (format (concat
             "Advise on pushing and pulling for the current repo state.\n"
             "Explain consequences/risks (e.g., diverged branches, rebases, merges), and suggest safe next steps.\n"
             "Return ONLY JSON with fields:\n"
             "  summary: string,\n"
             "  risks: array of strings,\n"
             "  suggestions: array of { title: string, commands: array of strings, keys: array of strings }\n"
             "You MAY include optional fields in suggestions items: rationale (string), steps (array of strings).\n"
             "Answer STRICTLY in %s for textual fields.\n"
             "%s"
             "\n--- STATUS -sb ---\n%s\n"
             "--- UPSTREAM ---\n%s (ahead %s, behind %s)\n"
             "--- REMOTES ---\n%s\n"
             "%s")
            ilang
            keys-block
            status up ahead behind rem
            (if (> (length recent) 0)
                (format "--- RECENT GIT OUTPUT ---\n%s\n" recent) ""))))

(defun magpt--render-explain-push-pull (json _data)
  (magpt--history-append-entry 'explain-push-pull (or magpt--current-request "") (or json "")
                               "JSON: {summary, risks[], suggestions[].commands[], suggestions[].keys[]}"))

;;;###autoload
(defun magpt-explain-push-pull ()
  "Run the 'Push/Pull advice' assist task (read-only)."
  (interactive)
  (magpt--ensure-assist-ready)
  (magpt-run-task 'explain-push-pull))

;; Branches overview

(defun magpt--ctx-branches-overview (_ctx)
  "Collect local branches, their upstreams and recent graph."
  (let* ((root (magpt--project-root))
         (branches (ignore-errors (magpt--git root "branch" "-vv" "--no-color")))
         (graph (ignore-errors (magpt--git root "log" "--oneline" "--decorate" "--graph" "--all" "-n" "40")))
         (keys (when magpt-include-magit-keys-in-suggestions
                 (magpt--format-magit-keys-cheatsheet-safe)))
         (preview (string-join
                   (delq nil
                         (list
                          (and branches (format "BRANCHES (-vv):\n%s" branches))
                          (and (and keys (> (length keys) 0))
                               (format "\n\nMAGIT KEYS:\n%s"
                                       (string-join (seq-take (split-string keys "\n" t) 50) "\n")))
                          (and graph (format "\n\nRECENT GRAPH:\n%s" graph))))
                   ""))
         (bytes (magpt--string-bytes preview)))
    (list (list :branches branches :graph graph :magit-keys keys)
          preview bytes)))

(defun magpt--prompt-branches-overview (data)
  "Build prompt to explain local/remote branches and safe actions."
  (let* ((ilang (or magpt-info-language "English"))
         (branches (or (plist-get data :branches) ""))
         (graph (or (plist-get data :graph) ""))
         (keys (plist-get data :magit-keys))
         (keys-block
          (if (and keys (stringp keys) (> (length keys) 0))
              (format "\nUse ONLY the Magit key bindings listed below for suggestions[].keys; otherwise use [].\n--- BEGIN MAGIT KEYS HELP ---\n%s\n--- END MAGIT KEYS HELP ---\n" keys)
            "\nIf relevant Magit key bindings are known, include them in suggestions[].keys; otherwise use [].\n")))
    (format (concat
             "Explain the current branches: which are current, tracking, ahead/behind, stale, or can be cleaned up.\n"
             "Suggest safe actions (switch, create, delete merged, set upstream, fetch/prune, rename).\n"
             "Return ONLY JSON with fields: summary, risks[], suggestions[].{title,commands[],keys[]}.\n"
             "You MAY include optional fields in suggestions items: rationale (string), steps (array of strings).\n"
             "Answer STRICTLY in %s.\n"
             "%s"
             "\n--- BRANCHES -vv ---\n%s\n"
             "\n--- RECENT GRAPH ---\n%s\n")
            ilang keys-block branches graph)))

(defun magpt--render-branches-overview (json _data)
  (magpt--history-append-entry 'explain-branches (or magpt--current-request "") (or json "")
                               "JSON: {summary, risks[], suggestions[]}"))

;;;###autoload
(defun magpt-explain-branches ()
  "Run the 'Branches overview' assist task (read-only)."
  (interactive)
  (magpt--ensure-assist-ready)
  (magpt-run-task 'explain-branches))

;; Recover file (how-to)

(defun magpt--ctx-restore-file-suggest (ctx)
  "Collect context to recover a file from a commit-ish. CTX may contain :path and :rev."
  (let* ((root (magpt--project-root))
         (default-path
          (when (and buffer-file-name (string-prefix-p (file-name-as-directory root)
                                                       (file-name-directory (expand-file-name buffer-file-name))))
            (file-relative-name (expand-file-name buffer-file-name) root)))
         (path (or (plist-get ctx :path)
                   (let* ((cands (ignore-errors (magpt--git root "ls-files" "-co" "--exclude-standard")))
                          (ls (and cands (split-string cands "\n" t))))
                     (completing-read "File to recover: " ls nil t nil nil default-path))))
         (rev  (or (plist-get ctx :rev)
                   (read-string "Recover from revision (commit-ish): " "HEAD")))
         (log (ignore-errors (magpt--git root "log" "--oneline" "--decorate" "--follow" "-n" "30" rev "--" path)))
         (keys (when magpt-include-magit-keys-in-suggestions
                 (magpt--format-magit-keys-cheatsheet-safe)))
         (preview (string-join
                   (delq nil
                         (list
                          (format "FILE: %s\nREV: %s" path rev)
                          (and (and keys (> (length keys) 0))
                               (format "\n\nMAGIT KEYS:\n%s"
                                       (string-join (seq-take (split-string keys "\n" t) 50) "\n")))
                          (and log (format "\n\nRECENT FILE HISTORY:\n%s" log))))
                   ""))
         (bytes (magpt--string-bytes preview)))
    (list (list :path path :rev rev :log log :magit-keys keys) preview bytes)))

(defun magpt--prompt-restore-file-suggest (data)
  "Build prompt that explains options to recover a file from a commit-ish."
  (let* ((ilang (or magpt-info-language "English"))
         (path (plist-get data :path))
         (rev (plist-get data :rev))
         (log (or (plist-get data :log) ""))
         (keys (plist-get data :magit-keys))
         (keys-block
          (if (and keys (stringp keys) (> (length keys) 0))
              (format "\nUse ONLY the Magit key bindings listed below for suggestions[].keys; otherwise use [].\n--- BEGIN MAGIT KEYS HELP ---\n%s\n--- END MAGIT KEYS HELP ---\n" keys)
            "\nIf relevant Magit key bindings are known, include them in suggestions[].keys; otherwise use [].\n")))
    (format (concat
             "I need to recover a file from a past commit. Explain safe options and provide commands.\n"
             "Cover: git restore --source=REV -- <path> (worktree and/or index), old 'git checkout REV -- <path>', and when to use each.\n"
             "Warn about overwriting unstaged/staged changes and how to stage selectively afterward.\n"
             "Return ONLY JSON with fields: summary, risks[], suggestions[].{title,commands[],keys[]}.\n"
             "Answer STRICTLY in %s.\n"
             "%s"
             "\n--- TARGET ---\nPATH: %s\nREV: %s\n"
             "\n--- RECENT FILE HISTORY ---\n%s\n")
            ilang keys-block path rev log)))

(defun magpt--render-restore-file-suggest (json data)
  (magpt--history-append-entry 'restore-file-suggest (or magpt--current-request "") (or json "")
                               "JSON: {summary, risks[], suggestions[]}"
                               :target-path (plist-get data :path)
                               :target-rev  (plist-get data :rev))
  )

;;;###autoload
(defun magpt-restore-file-suggest (&optional path rev)
  "Run 'Recover file (how-to)' assist task; prompts for PATH/REV if not provided."
  (interactive)
  (magpt--ensure-assist-ready)
  (magpt-run-task 'restore-file-suggest (list :path path :rev rev)))

(defvar magpt--assist-tasks-registered nil
  "Non-nil when assist tasks have been registered in the registry.")

(defun magpt--register-assist-tasks ()
  "Register Phase 1 assist tasks into the registry (read-only tasks)."
  (unless magpt--assist-tasks-registered
    ;; Explain Status (no confirm)
    (magpt-register-task
     (magpt--task :name 'explain-status
                  :title "Explain current status"
                  :scope 'repo
                  :context-fn #'magpt--ctx-status
                  :prompt-fn  #'magpt--prompt-explain-status
                  :render-fn  #'magpt--render-explain-status
                  :apply-fn   nil
                  :confirm-send? nil))
    ;; Commit Lint/Fix Suggest
    (magpt-register-task
     (magpt--task :name 'commit-lint-suggest
                  :title "Commit Lint / Fix Suggest"
                  :scope 'repo
                  :context-fn #'magpt--ctx-commit-lint
                  :prompt-fn  #'magpt--prompt-commit-lint
                  :render-fn  #'magpt--render-commit-lint
                  :apply-fn   nil
                  :confirm-send? t))
    ;; Branch Name Suggest
    (magpt-register-task
     (magpt--task :name 'branch-name-suggest
                  :title "Branch Name Suggest"
                  :scope 'repo
                  :context-fn #'magpt--ctx-branch-name
                  :prompt-fn  #'magpt--prompt-branch-name
                  :render-fn  #'magpt--render-branch-name
                  :apply-fn   nil
                  :confirm-send? t))
    ;; Push/Pull advice
    (magpt-register-task
     (magpt--task :name 'explain-push-pull
                  :title "Push/Pull advice"
                  :scope 'repo
                  :context-fn #'magpt--ctx-push-pull
                  :prompt-fn  #'magpt--prompt-explain-push-pull
                  :render-fn  #'magpt--render-explain-push-pull
                  :apply-fn   nil
                  :confirm-send? t))
    ;; Branches overview
    (magpt-register-task
     (magpt--task :name 'explain-branches
                  :title "Branches overview"
                  :scope 'repo
                  :context-fn #'magpt--ctx-branches-overview
                  :prompt-fn  #'magpt--prompt-branches-overview
                  :render-fn  #'magpt--render-branches-overview
                  :apply-fn   nil
                  :confirm-send? t))
    ;; Recover file (how-to)
    (magpt-register-task
     (magpt--task :name 'restore-file-suggest
                  :title "Recover file (how-to)"
                  :scope 'repo
                  :context-fn #'magpt--ctx-restore-file-suggest
                  :prompt-fn  #'magpt--prompt-restore-file-suggest
                  :render-fn  #'magpt--render-restore-file-suggest
                  :apply-fn   nil
                  :confirm-send? t))
    (setq magpt--assist-tasks-registered t)))

(defun magpt--ensure-assist-ready ()
  "Ensure registry is enabled and assist tasks are registered."
  (magpt--log "ensure-assist-ready: begin enable=%S" magpt-enable-task-registry)
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use assist tasks (Phase 1)"))
  ;; Ensure core is loaded (registry helpers, structs) and also recommend tasks,
  ;; so . g works even if order of loads was unusual.
  (require 'magpt nil t)
  (require 'magpt-tasks-recommend nil t)
  ;; Register Assist tasks, and also Recommend tasks (harmless if duplicates).
  (magpt--register-assist-tasks)
  (when (fboundp 'magpt--register-recommend-tasks)
    (magpt--register-recommend-tasks))
  ;; Log current registry; if empty, try once more defensively.
  (let ((ks (magpt--hash-table-keys magpt--tasks)))
    (magpt--log "ensure-assist-ready: done; tasks=%s"
                (mapcar #'symbol-name ks))
    (unless (memq 'explain-status ks)
      (magpt--log "ensure-assist-ready: WARN: explain-status not registered; forcing re-register")
      (setq magpt--assist-tasks-registered nil)
      (magpt--register-assist-tasks)
      (magpt--log "ensure-assist-ready: after force; tasks=%s"
                  (mapcar #'symbol-name (magpt--hash-table-keys magpt--tasks))))))

;;;###autoload
(defun magpt-explain-status ()
  "Run the 'Explain Status' task and update AI overview in Magit (read-only)."
  (interactive)
  (magpt--log "key [. g]: magpt-explain-status invoked buffer=%s root=%s"
              (buffer-name)
              (ignore-errors (magpt--project-root)))
  (condition-case magpt--e
      (progn
        (magpt--log "explain-status: ensure start")
        (magpt--ensure-assist-ready)
        (magpt--log "explain-status: ensure OK; dispatching run-task")
        (magpt-run-task 'explain-status)
        (magpt--log "explain-status: run-task dispatched"))
    (quit
     ;; Closing/cancelling Transient — keep logs quiet.
     (magpt--log "explain-status: quit (transient closed)"))
    (error
     (let* ((emsg (condition-case _ (error-message-string magpt--e)
                    (error "<no-error-object>")))
            (esym (car-safe magpt--e))
            (edata (cdr-safe magpt--e)))
       (magpt--log "explain-status: ERROR: %s" emsg)
       (magpt--log "explain-status: signal=%S data=%S" esym edata)
       (magpt--log "explain-status: buffer=%s default-directory=%s"
                   (buffer-name) default-directory)
       (magpt--log "explain-status: BT:\n%s" (magpt--backtrace-string))))))
;;;###autoload
(defun magpt-commit-lint-suggest ()
  "Run the 'Commit Lint/Fix Suggest' task and update AI overview in Magit (read-only)."
  (interactive)
  (magpt--ensure-assist-ready)
  (magpt-run-task 'commit-lint-suggest))

;;;###autoload
(defun magpt-branch-name-suggest ()
  "Run the 'Branch Name Suggest' task and update AI overview in Magit (read-only)."
  (interactive)
  (magpt--ensure-assist-ready)
  (magpt-run-task 'branch-name-suggest))

(provide 'magpt-tasks-assist)

;;; magpt-tasks-assist.el ends here
