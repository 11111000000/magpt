;;; magpt-tasks-recommend.el --- Recommendation (Phase 2) tasks for MaGPT  -*- lexical-binding: t; -*-

;; Author: Peter
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, vc, git, ai
;; URL: https://github.com/11111000000/magpt

;;; Commentary:
;; Phase 2 recommend tasks (Explain Hunk/Region, Stage by Intent, Range Summary).
;; Real implementations (context → prompt → render) extracted from magpt.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit nil t)

;; Forward declarations to magpt core
(declare-function magpt--maybe-load-rc "ext:magpt")
(declare-function magpt--project-root "magpt-git")
(declare-function magpt--git "magpt-git" (dir &rest args))
(declare-function magpt--string-bytes "magpt-git" (s))
(declare-function magpt--i18n "ext:magpt" (key &rest args))
(declare-function magpt--history-append-entry "magpt-history" (task request response &optional note &rest kvs))
(declare-function magpt-register-task "ext:magpt" (task))
(declare-function magpt--task "ext:magpt" (&rest args))
(declare-function magpt--hash-table-keys "ext:magpt" (ht))
(declare-function magpt-run-task "ext:magpt" (name &optional ctx))

;; Explain Hunk/Region

(defun magpt--ctx-hunk-or-region (_ctx)
  "Return (data preview bytes) for current region in a file buffer or Magit diff hunk."
  (cond
   ((and (region-active-p) buffer-file-name)
    (let* ((beg (region-beginning))
           (end (region-end))
           (text (buffer-substring-no-properties beg end))
           (line-beg (line-number-at-pos beg))
           (line-end (line-number-at-pos end))
           (data (list :kind 'region
                       :file (abbreviate-file-name buffer-file-name)
                       :start line-beg :end line-end
                       :text text)))
      (list data text (magpt--string-bytes text))))
   ((and (derived-mode-p 'magit-diff-mode))
    (save-excursion
      (let (hstart hend)
        (unless (re-search-backward "^@@ " nil t)
          (user-error "Place point inside a diff hunk (header starts with @@)"))
        (setq hstart (line-beginning-position))
        (if (re-search-forward "^@@ \\|^diff --git " nil t)
            (setq hend (line-beginning-position))
          (setq hend (point-max)))
        (let* ((text (buffer-substring-no-properties hstart hend))
               (data (list :kind 'hunk :file "<diff>" :text text)))
          (list data text (magpt--string-bytes text))))))
   (t
    (user-error "Select a region in a file or place point on a Magit diff hunk"))))

(defun magpt--prompt-explain-hunk (data)
  "Build prompt for explaining a code change (region or diff hunk) with DATA."
  (let ((ilang (or magpt-info-language "English"))
        (text (or (plist-get data :text) "")))
    (format (concat
             "Explain the following code change concisely.\n"
             "Return ONLY JSON with fields:\n"
             "  summary: string,\n"
             "  rationale: string,\n"
             "  risks: array of strings\n"
             "Answer STRICTLY in %s for all textual fields. No Markdown outside JSON.\n\n"
             "--- BEGIN CONTEXT ---\n%s\n--- END CONTEXT ---\n")
            ilang
            text)))

(defun magpt--render-explain-hunk (json _data)
  (magpt--history-append-entry 'explain-hunk-region (or magpt--current-request "") (or json "")
                               "JSON: {summary, rationale, risks[]}"))

(defvar magpt--recommend-tasks-registered nil
  "Non-nil when recommend (Phase 2) tasks have been registered.")

(defun magpt--register-recommend-tasks ()
  "Register Phase 2 recommend tasks."
  (unless magpt--recommend-tasks-registered
    ;; Explain Hunk/Region
    (magpt-register-task
     (magpt--task :name 'explain-hunk-region
                  :title "Explain Hunk/Region"
                  :scope 'file
                  :context-fn #'magpt--ctx-hunk-or-region
                  :prompt-fn  #'magpt--prompt-explain-hunk
                  :render-fn  #'magpt--render-explain-hunk
                  :apply-fn   nil
                  :confirm-send? t))
    ;; Stage by Intent (files)
    (magpt-register-task
     (magpt--task :name 'stage-by-intent
                  :title "Stage by Intent (groups)"
                  :scope 'repo
                  :context-fn #'magpt--ctx-stage-intent
                  :prompt-fn  #'magpt--prompt-stage-intent
                  :render-fn  #'magpt--render-stage-intent
                  :apply-fn   nil
                  :confirm-send? t))
    ;; Stage by Intent (hunks via unified diff)
    (magpt-register-task
     (magpt--task :name 'stage-by-intent-hunks
                  :title "Stage by Intent (hunks via patch)"
                  :scope 'repo
                  :context-fn #'magpt--ctx-stage-intent-hunks
                  :prompt-fn  #'magpt--prompt-stage-intent-hunks
                  :render-fn  #'magpt--render-stage-intent-hunks
                  :apply-fn   nil
                  :confirm-send? t))
    ;; PR/Range Summary
    (magpt-register-task
     (magpt--task :name 'range-summary
                  :title "PR/Range Summary"
                  :scope 'repo
                  :context-fn #'magpt--ctx-range-summary
                  :prompt-fn  #'magpt--prompt-range-summary
                  :render-fn  #'magpt--render-range-summary
                  :apply-fn   nil
                  :confirm-send? t))
    (setq magpt--recommend-tasks-registered t)))

;;;###autoload
(defun magpt-explain-hunk-region ()
  "Run 'Explain Hunk/Region' (Phase 2, read-only)."
  (interactive)
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use experimental tasks"))
  (magpt--register-recommend-tasks)
  (magpt-run-task 'explain-hunk-region))

;; Stage by Intent (hunks via unified diff)

(defun magpt--ctx-stage-intent-hunks (_ctx)
  "Collect context for hunk-level staging patch suggestion."
  (let* ((root (magpt--project-root))
         (porc (magpt--git root "status" "--porcelain"))
         (diff (magpt--git root "diff" "--no-color"))
         (preview (format "STATUS:\n%s\n\nDIFF:\n%s"
                          (string-join (seq-take (split-string porc "\n" t) 200) "\n")
                          (if (> (length diff) 8000) (concat (substring diff 0 8000) " …") diff)))
         (bytes (magpt--string-bytes preview)))
    (list (list :porcelain porc :diff diff) preview bytes)))

(defun magpt--prompt-stage-intent-hunks (data)
  "Build prompt for hunk-level staging patch suggestion."
  (let ((ilang (or magpt-info-language "English")))
    (format (concat
             "Produce a minimal unified diff to STAGE only the most coherent hunks.\n"
             "Rules:\n"
             "- Output ONLY a valid unified diff (no prose, no Markdown).\n"
             "- Base is the current working tree; the patch MUST apply with `git apply --cached --check`.\n"
             "- Prefer safe, small steps; do not include unrelated hunks.\n"
             "Answer STRICTLY in %s (only if any textual fields appear; usually none).\n\n"
             "--- BEGIN STATUS ---\n%s\n--- END STATUS ---\n\n"
             "--- BEGIN DIFF ---\n%s\n--- END DIFF ---\n")
            ilang
            (plist-get data :porcelain)
            (plist-get data :diff))))

(defun magpt--render-stage-intent-hunks (patch _data)
  (magpt--history-append-entry 'stage-by-intent-hunks (or magpt--current-request "") (or patch "")
                               "Unified diff; check via git apply --cached --check"))

;;;###autoload
(defun magpt-stage-by-intent-hunks ()
  "Request a minimal unified diff to stage selected hunks (safe preview)."
  (interactive)
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use experimental tasks"))
  (magpt--register-recommend-tasks)
  (magpt-run-task 'stage-by-intent-hunks))

;; Stage by Intent (files)

(defun magpt--ctx-stage-intent (_ctx)
  "Collect porcelain status for stage-by-intent task."
  (let* ((root (magpt--project-root))
         (porc (magpt--git root "status" "--porcelain"))
         (bytes (magpt--string-bytes porc)))
    (list porc porc bytes)))

(defun magpt--prompt-stage-intent (porcelain)
  "Build prompt for stage-by-intent."
  (let ((ilang (or magpt-info-language "English")))
    (format (concat
             "Group the following Git changes into a few logical groups for staging.\n"
             "Return ONLY JSON with:\n"
             "  groups: array of { title: string, rationale: string, files: array of { path: string, action: \"stage\"|\"unstage\" } }\n"
             "Constraints: only whole-file actions (no hunks). Prefer minimal, safe steps.\n"
             "Answer STRICTLY in %s for textual fields.\n\n"
             "--- BEGIN PORCELAIN ---\n%s\n--- END PORCELAIN ---\n")
            ilang porcelain)))

(defun magpt--render-stage-intent (json _data)
  (magpt--history-append-entry 'stage-by-intent (or magpt--current-request "") (or json "")
                               "JSON: {groups[].files[{path,action:stage|unstage}]}"))

;;;###autoload
(defun magpt-stage-by-intent ()
  "Run 'Stage by Intent' (Phase 2) and append plan to history (read-only)."
  (interactive)
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use experimental tasks"))
  (magpt--register-recommend-tasks)
  (magpt-run-task 'stage-by-intent))

;; Range summary

(declare-function magit-read-range "ext:magit" (&optional prompt))

(defun magpt--read-range-default ()
  "Read a commit RANGE using Magit if available; otherwise prompt."
  (if (and (featurep 'magit) (fboundp 'magit-read-range))
      (magit-read-range "Range for summary")
    (read-string "Range (e.g., HEAD~5..HEAD): " "HEAD~5..HEAD")))

(defun magpt--ctx-range-summary (range)
  "Collect LOG/STAT for RANGE. Return (data preview bytes)."
  (let* ((root (magpt--project-root))
         (rng (or range (magpt--read-range-default)))
         (log (magpt--git root "log" "--no-color" "--date=short"
                          "--pretty=%h %ad %an %s" rng))
         (stat (magpt--git root "log" "--no-color" "--stat" "--oneline" rng))
         (preview (format "RANGE: %s\n\nLOG:\n%s\n\nSTAT:\n%s"
                          rng
                          (if (> (length log) 4000) (concat (substring log 0 4000) " …") log)
                          (string-join (seq-take (split-string stat "\n" t) 200) "\n")))
         (bytes (magpt--string-bytes preview)))
    (list (list :range rng :log log :stat stat) preview bytes)))

(defun magpt--prompt-range-summary (data)
  "Build prompt for PR/Range Summary using DATA (:range :log :stat)."
  (let ((ilang (or magpt-info-language "English")))
    (format (concat
             "Draft a concise Pull Request description for the given commit range.\n"
             "Return ONLY JSON with fields:\n"
             "  title: string,\n"
             "  summary: string,\n"
             "  highlights: array of strings,\n"
             "  checklist: array of strings\n"
             "Answer STRICTLY in %s.\n\n"
             "--- RANGE ---\n%s\n\n--- LOG ---\n%s\n\n--- STAT ---\n%s\n")
            ilang
            (plist-get data :range)
            (plist-get data :log)
            (plist-get data :stat))))

(defun magpt--render-range-summary (json _data)
  "Append PR/Range Summary JSON into history."
  (magpt--history-append-entry 'range-summary (or magpt--current-request "") (or json "")
                               "JSON: {title, summary, highlights[], checklist[]}"))

;;;###autoload
(defun magpt-range-summary (&optional range)
  "Run 'PR/Range Summary' task and append result to history."
  (interactive)
  (magpt--maybe-load-rc)
  (unless magpt-enable-task-registry
    (user-error "Enable `magpt-enable-task-registry' to use experimental tasks"))
  (magpt--register-recommend-tasks)
  (magpt-run-task 'range-summary range))

(provide 'magpt-tasks-recommend)

;;; magpt-tasks-recommend.el ends here
