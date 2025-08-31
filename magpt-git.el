;;; magpt-git.el --- Git plumbing helpers for MaGPT  -*- lexical-binding: t; -*-

;; Author: Peter
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, vc, git
;; URL: https://github.com/11111000000/magpt

;;; Commentary:
;; Encapsulate git command invocation, root discovery and a small per-repo
;; recent-output cache used by Explain Status.

;;; Code:

(require 'subr-x)
(require 'seq)

;; External deps from core (magpt.el)
(declare-function magpt--log "ext:magpt" (fmt &rest args))


(defcustom magpt-recent-git-output-lines 80
  "How many last lines of Git output to keep per repository."
  :type 'integer
  :group 'magpt)

(defvar magpt--recent-git-output-map (make-hash-table :test 'equal)
  "Map ROOT-KEY → last lines of git output (newline-separated string).")

(defun magpt--recent-git--key (dir)
  "Return the map key for recent output by directory DIR."
  (file-name-as-directory (expand-file-name (or dir default-directory))))

(defun magpt--recent-git-output-get (dir)
  "Return saved recent output for directory DIR (as a string)."
  (or (gethash (magpt--recent-git--key dir) magpt--recent-git-output-map) ""))

(defun magpt--recent-git-output-append (dir exit args out)
  "Append an entry for DIR to the log with EXIT, command ARGS, and output OUT."
  (let* ((key (magpt--recent-git--key dir))
         (hdr (format "%d git %s" exit (mapconcat #'identity args " ")))
         (lines (append (list hdr)
                        (and (stringp out) (split-string out "\n" t))))
         (prev (and (gethash key magpt--recent-git-output-map)
                    (split-string (gethash key magpt--recent-git-output-map) "\n" t)))
         (all (append (or prev '()) lines))
         (keep (last all (max 1 magpt-recent-git-output-lines))))
    (puthash key (string-join keep "\n") magpt--recent-git-output-map)))

(defun magpt--executable-git ()
  "Return the path to the git executable or nil."
  (executable-find "git"))

(defun magpt--process-git (dir &rest args)
  "Execute git with ARGS in DIR. Return (EXIT-CODE . STRING-OUTPUT)."
  (let ((default-directory (file-name-as-directory (or dir default-directory))))
    (with-temp-buffer
      (let* ((start (float-time)))
        (magpt--log "process-git: start dir=%s args=%s"
                    default-directory (mapconcat #'identity args " "))
        (let* ((exit (apply #'process-file (or (magpt--executable-git) "git")
                            nil t nil args))
               (out  (buffer-string))
               (trimmed (if (string-suffix-p "\n" out) (substring out 0 -1) out))
               (dur (- (float-time) start)))
          (magpt--log "process-git: end exit=%s dur=%.3fs bytes=%d"
                      exit dur (length trimmed))
          ;; Record into per-repo log (for Explain Status → RECENT GIT OUTPUT)
          (ignore-errors (magpt--recent-git-output-append default-directory exit args trimmed))
          (cons exit trimmed))))))

(defun magpt--git (dir &rest args)
  "Execute git ARGS in DIR and return the output string or signal user-error."
  (pcase (apply #'magpt--process-git dir args)
    (`(,exit . ,out)
     (if (zerop exit)
         out
       (user-error "Git error (%s): %s" exit out)))))

(defun magpt--git-apply-temp (dir patch &rest args)
  "Apply PATCH (string) via 'git apply' in DIR with ARGS, using a temp file.
Signal a user-error on non-zero exit."
  (let ((tmp (make-temp-file "magpt-patch" nil ".patch")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (string-trim-right (or patch "")) "\n"))
          (apply #'magpt--git dir "apply" (append args (list tmp))))
      (ignore-errors (delete-file tmp)))))

(defun magpt--git-apply-check-temp (dir patch &rest args)
  "Run 'git apply --check' for PATCH (string) in DIR with ARGS.
Return (EXIT-CODE . OUTPUT) without signaling."
  (let ((tmp (make-temp-file "magpt-patch" nil ".patch")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (string-trim-right (or patch "")) "\n"))
          (apply #'magpt--process-git dir "apply" (append '("--check") args (list tmp))))
      (ignore-errors (delete-file tmp)))))

(defun magpt--git-root-from (dir)
  "Return the Git repo root directory for DIR, or nil if not found."
  (when (and (magpt--executable-git) (file-directory-p dir))
    (let ((res (magpt--process-git dir "rev-parse" "--show-toplevel")))
      (when (eq (car res) 0)
        (file-name-as-directory (cdr res))))))

(defun magpt--try-root-from-magit ()
  "Return repo root using Magit, or nil if unavailable."
  (when (and (featurep 'magit) (fboundp 'magit-toplevel))
    (ignore-errors
      (let ((root (magit-toplevel)))
        (when (and (stringp root) (file-directory-p root))
          (file-name-as-directory root))))))

(defun magpt--try-root-from-vc ()
  "Return repo root using VC, or nil if unavailable."
  (let ((root (ignore-errors (vc-root-dir))))
    (when (and (stringp root) (file-directory-p root))
      (file-name-as-directory root))))

(defun magpt--try-root-from-project ()
  "Return project root using project.el, or nil if unavailable."
  (let* ((proj (ignore-errors (project-current)))
         (root (when proj (ignore-errors (project-root proj)))))
    (when (and (stringp root) (file-directory-p root))
      (file-name-as-directory root))))

(defun magpt--project-root ()
  "Determine the Git project root according to `magpt-project-root-strategy'.
Signal an error if no repository is found."
  (let* ((candidates
          (pcase magpt-project-root-strategy
            ('prefer-magit
             (list #'magpt--try-root-from-magit
                   #'magpt--try-root-from-vc
                   #'magpt--try-root-from-project))
            ('prefer-vc
             (list #'magpt--try-root-from-vc
                   #'magpt--try-root-from-magit
                   #'magpt--try-root-from-project))
            ('prefer-project
             (list #'magpt--try-root-from-project
                   #'magpt--try-root-from-magit
                   #'magpt--try-root-from-vc))
            (_ (list #'magpt--try-root-from-magit
                     #'magpt--try-root-from-vc
                     #'magpt--try-root-from-project))))
         (root-s (or (seq-some (lambda (f) (funcall f)) candidates)
                     (and (file-directory-p default-directory)
                          (magpt--git-root-from default-directory)))))
    (unless root-s
      (user-error "No Git repository found for current directory"))
    (file-name-as-directory (expand-file-name root-s))))

(defun magpt--staged-diff (root)
  "Return the diff string for staged changes in ROOT."
  (apply #'magpt--git root "diff" (or magpt-diff-args '("--staged" "--no-color"))))

(defun magpt--string-bytes (s)
  "Return UTF-8 byte size of string S."
  (if (stringp s) (string-bytes s) 0))

(provide 'magpt-git)

;;; magpt-git.el ends here
