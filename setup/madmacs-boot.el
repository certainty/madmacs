;; -*- lexical-binding: t; -*-

(defmacro madmacs--measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(progn ,@body)
   ;(require 'benchmark)
  ;; `(let ((elapsed (benchmark-elapse ,@body)))
  ;;    (message ";; ========================== elapsed time: %.06f ===============================" elapsed))

  )

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

(defun madmacs--load-files (context files)
  "Loads all files in FILES in logging that as context and measuring the time it takes"
  (message ";; ========================== %s ======================" context)
  (madmacs--measure-time
   (dolist (file files)
     (require file nil (not madmacs-debug)))))

(defun madmacs--setup-env ()
  "Loads the environment variables from the madmacs.env if it exists"
  (let ((env-file (expand-file-name "madmacs.env" user-emacs-directory)))
    (when (file-exists-p env-file)
      (message ";; Loading environment variables from %s" env-file)
      (with-temp-buffer
        (insert-file-contents env-file)
        (while (not (eobp))
          (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
            (when (string-match "^\\([^=]+\\)=\"\\(.*\\)\"$" line)
              (let ((variable (match-string 1 line))
                    (value (match-string 2 line)))
                (message ";; Setting env var %s to %s" variable value)
                (setenv variable value)
                (when (string= variable "PATH")
                  (setq exec-path (append exec-path (list value))))))
            (forward-line 1)))))))

(defun madmacs--boot-early ()
    "Run the early boot process"
    (message ";; We are all mad here .....")
    (madmacs--setup-native-compilation)
    (madmacs--load-files "Madmacs Boot" '(madmacs-early-packages madmacs-early-ui))

    (when madmacs-early-init-features
      (madmacs--load-files "Madmacs Early Init" madmacs-early-init-features)))

(defun madmacs--after-init ()
    "Run the after init boot process"
    (madmacs--load-files "Madmacs After Init" madmacs-after-init-features))

(defun madmacs--after-startup ()
  "Run the after startup boot process"
  (madmacs--load-files "Madmacs After Startup" madmacs-after-startup-features))

(defun madmacs--boot ()
  "Boot the madmacs configuration"
  (require 'madmacs-init-packages)
  (require 'madmacs-libraries)

  (madmacs--load-files "Madmacs Init" madmacs-init-features)

  (add-hook 'after-init-hook  'madmacs--after-init)
  (add-hook 'emacs-startup-hook 'madmacs--after-startup))

(provide 'madmacs-boot)
