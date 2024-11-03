;; -*- lexical-binding: t; -*-

(defconst madmacs--sys-mac-p (eq system-type 'darwin))
(defconst madmacs--sys-linux-p (eq system-type 'gnu/linux))

(defgroup madmacs nil
    "Madmacs configuration group"
    :prefix "madmacs-"
    :group 'emacs)

(defvar madmacs-early-init-features nil
    "These packages are loaded as part of the early init process")

(defvar madmacs-init-features nil
    "These packages are loaded as part of the init process directly after the early init process")

(defvar madmacs-after-init-features nil
    "These packages are loaded as an after-init-hook")

(defvar madmacs-after-startup-features nil
    "These packages are loaded as an emacs-startup-hook")

(defcustom madmacs-debug nil
    "Enable debug mode"
    :type 'boolean
    :group 'madmacs)

(defcustom madmacs-default-font "Iosevka Nerd Font"
    "Default font to use"
    :type 'string
    :group 'madmacs)

(defcustom madmacs-variable-pitch-font "Iosevka Aile"
  "Variable pitch font to use"
  :type 'string
  :group 'madmacs)

(defcustom madmacs-theme 'modus
  "The theme to activate"
  :type 'symbol
  :options '(modus catppuccin))

(defcustom madmacs-default-font-size 130
    "Default font size to use"
    :type 'integer
    :group 'madmacs)

(defcustom madmacs-presentation-font-size 190
    "Presentation font size to use"
    :type 'integer
    :group 'madmacs)

(defcustom madmacs-presentation-bigger-font-size 210
    "Bigger presentation font size to use"
    :type 'integer
    :group 'madmacs)

(defcustom madmacs-lsp-client 'eglot
    "The LSP client to use."
    :type 'symbol
    :options '(eglot lsp-mode)
    :group 'madmacs)

(defcustom madmacs-modal-approach 'meow
  "The modal approach to take for this lovely emacs"
  :type 'symbol
  :options '(evil meow boon)
  :group 'madmacs)

(defcustom madmacs-user-full-name ""
    "User full name"
    :type 'string
    :group 'madmacs)

(defcustom madmacs-user-email ""
    "User email"
    :type 'string
    :group 'madmacs)

(defcustom madmacs--base-dir-name "madmacs"
    "Base directory name for madmacs configuration, cache, state and other files. We use XDG base directory specification and will group all madmacs related files under this directory"
    :type 'string
    :group 'madmacs)

(defvar madmacs--user-home-dir (expand-file-name "~")
    "Directory where we store user specific configuration files")

(defvar madmacs--cache-dir (expand-file-name (concat madmacs--user-home-dir (file-name-as-directory "/.cache") madmacs--base-dir-name))
    "Directory where we store cache files")

(defvar madmacs--modules-dir (expand-file-name "modules" user-emacs-directory)
    "Directory where we store configuration files")

(defvar madmacs--setup-dir (expand-file-name "setup" user-emacs-directory)
    "Directory where we store setup files")

(defvar madmacs--data-dir (expand-file-name (concat madmacs--user-home-dir (file-name-as-directory "/.local/share") madmacs--base-dir-name))
    "Directory where we store data files")

(defcustom madmacs-org-directory (expand-file-name "org" madmacs--data-dir)
    "Directory where we store org files"
    :type 'string
    :group 'madmacs)

(defun madmacs--defaults-with-overrides (defaults overrides)
    "Merge overrides into defaults. Overriding any defaults."
    (append (cl-remove-if (lambda (x) (member (car x) (mapcar 'car overrides))) defaults) overrides))

(use-package emacs
    :custom
    (user-full-name madmacs-user-full-name)
    (user-mail-address madmacs-user-email)
    (custom-file (expand-file-name "custom.el" madmacs--data-dir)))

(provide 'madmacs-settings)
