;;; magpt-ui-icons.el --- Emoji icons for MaGPT UI -*- lexical-binding: t; -*-

;; Centralized emoji icons mapping and helper.

;;; Code:

(defgroup magpt-ui nil
  "User interface settings for MaGPT."
  :group 'magpt)

(defcustom magpt-ui-icons-enabled t
  "If non-nil, show emoji icons in MaGPT UI."
  :type 'boolean
  :group 'magpt-ui)

(defcustom magpt-ui-emoji-map
  '((suggestions . "💡")
    (suggestion-default . "💡")
    (summary . "ℹ️")
    (risk . "⚠️")
    (commit-lint . "✅")
    (branch-name . "🌿")
    (resolve-conflict . "🔀")
    (push-pull . "🔄")
    (branches-overview . "🌳")
    (restore-file . "↩️")
    (reset-files . "🕓")
    (stash . "📦")
    (undo-commits . "⏪")
    (reflog-rescue . "🛟")
    (detached-head . "🧩")
    (set-upstream . "🡕")
    (eshell-button . "⌨️"))
  "Emoji icons used by MaGPT UI."
  :type '(alist :key-type symbol :value-type string)
  :group 'magpt-ui)

(defun magpt--icon (key)
  "Return emoji icon string for KEY when icons are enabled; empty string otherwise."
  (if (and (boundp 'magpt-ui-icons-enabled) magpt-ui-icons-enabled)
      (or (alist-get key magpt-ui-emoji-map) "")
    ""))

(provide 'magpt-ui-icons)

;;; magpt-ui-icons.el ends here
