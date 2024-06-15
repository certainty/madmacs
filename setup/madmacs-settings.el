(defconst madmacs--sys-mac-p (eq system-type 'darwin))
(defconst madmacs--sys-linux-p (eq system-type 'gnu/linux))

(defgroup madmacs nil
  "Madmacs configuration group"
  :prefix "madmacs-"
  :group 'emacs)

(defvar madmacs-run-level-1-features nil
    "The packages to load for run-level 1 which is is executed immediately after early-init")

(defvar madmacs-run-level-2-features nil
  "The packages to load for run-level 2 which is executed as after-init hook")

(defvar madmacs-run-level-3-features nil
  "The packages to load for run-level 3 which is executed as emacs-startup-hook")

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

(defcustom madmacs-default-font-size 140
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

(defun madmacs--defaults-with-overrides (defaults overrides)
  "Merge overrides into defaults. Overriding any defaults."
  (append (cl-remove-if (lambda (x) (member (car x) (mapcar 'car overrides))) defaults) overrides))

(use-package emacs
  :custom
  (user-full-name "David Krentzlin")
  (user-mail-address "david.krentzlin@gmail.com")
  (custom-file (expand-file-name "custom.el" user-emacs-directory)))

(provide 'madmacs-settings)