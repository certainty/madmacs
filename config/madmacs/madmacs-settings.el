(defconst madmacs--sys-mac (eq system-type 'darwin))
(defconst madmacs--sys-linux (eq system-type 'gnu/linux))

(defvar madmacs--runlevel-files-alist
  '((0 . ())
    (1 . ())
    (2 . ())
    (3 . ()))
  "Alist of run-levels and the files that should be loaded at that run-level")

(defgroup madmacs nil
  "Madmacs configuration group"
  :prefix "madmacs-"
  :group 'emacs)

(defcustom madmacs-boot-run-level 3
  "Stop after a the specified boot phase, which is a symbol out of a number from 0 to 4"
   :type '(choice (const :tag "No stop" nil)
                  (const :tag "Stop loading anything after run-level 0 which is just the early-init process" 0)
                  (const :tag "Stop loading anything after run-level 1 " 1)
                  (const :tag "Stop loading anything after run-level 2" 2)
                  (const :tag "Stop loading anything after run-level 3" 3))
   :group 'madmacs)

(defcustom madmacs-debug nil
  "Enable debug mode"
  :type 'boolean
  :group 'madmacs)

(defcustom madmacs--base-dir-name "madmacs"
  "Base directory name for madmacs configuration, cache, state and other files. We use XDG base directory specification and will group all madmacs related files under this directory"
   :type 'string
   :group 'madmacs)


(defvar madmacs--user-home-dir (expand-file-name "~")
  "Directory where we store user specific configuration files")

(defvar madmacs--cache-dir (expand-file-name (concat madmacs--user-home-dir (file-name-as-directory "/.cache") madmacs--base-dir-name))
  "Directory where we store cache files")

(defvar madmacs--data-dir (expand-file-name (concat madmacs--user-home-dir (file-name-as-directory "/.local/share") madmacs--base-dir-name))
  "Directory where we store data files")


(use-package emacs
  :custom
  (user-full-name "David Krentzlin")
  (user-mail-address "david.krentzlin@gmail.com")
  (custom-file (expand-file-name "custom.el" user-emacs-directory)))

(provide 'madmacs-settings)
