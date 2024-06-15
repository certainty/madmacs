;; Establish very early customization
(when (version< emacs-version "29")
  (error "Madmacs requires Emacs 29 or later"))

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


(defmacro madmacs--measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  (require 'benchmark)
  `(let ((elapsed (benchmark-elapse ,@body)))
     (message "Elapsed time: %.06f" elapsed)))

(defvar madmacs--user-home-dir (expand-file-name "~")
  "Directory where we store user specific configuration files")

(defvar madmacs--cache-dir (expand-file-name (concat madmacs--user-home-dir (file-name-as-directory "/.cache") madmacs--base-dir-name))
  "Directory where we store cache files")

(defvar madmacs--data-dir (expand-file-name (concat madmacs--user-home-dir (file-name-as-directory "/.local/share") madmacs--base-dir-name))
  "Directory where we store data files")

(defun madmacs--ensure-cache-dir (path)
    "Ensure that the cache directory exists"
    (let ((dir (expand-file-name path madmacs--cache-dir)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      dir))

(defun madmacs--ensure-data-dir (path)
    "Ensure that the data directory exists"
    (let ((dir (expand-file-name path madmacs--data-dir)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      dir))

(defun madmacs--setup-native-compilation ()
  "Setup native compilation and make sure the emacs we're running in supports it"
  (unless (and (fboundp 'native-comp-available-p) (native-comp-available-p))
    (error "Native compilation is not supported in this version of Emacs"))

  (let ((eln-cache-dir (madmacs--ensure-cache-dir "eln-cache")))
    (startup-redirect-eln-cache eln-cache-dir))

  (setopt native-comp-speed 2)
  (setopt native-comp-deferred-compilation t))

(defun madmacs--report-run-level (run-level)
  "Report the run-level we're at"
  (message ";; =================== Madmacs run-level: %d" run-level))

(defun madmacs--run-level (run-level)
  "Run the configuration for the specified run-level"
  (madmacs--report-run-level run-level)

  (madmacs--measure-time
   (dolist (file (alist-get run-level madmacs--runlevel-files-alist))
     (require file nil (not madmacs-debug)))))

(defun madmacs--pre-boot ()
  "Run early init boot process aka run-level 0"
  (madmacs--setup-native-compilation)
  (madmacs--run-level 0))

(defun madmacs--boot ()
  "Run the full boot process up until the configured `madmacs-boot-run-level' boot process"

  (cl-loop for i from 1 to madmacs-boot-run-level
           do (madmacs--run-level i)))

(provide 'madmacs-boot)
