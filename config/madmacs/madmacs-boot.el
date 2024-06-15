(defmacro madmacs--measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  (require 'benchmark)
  `(let ((elapsed (benchmark-elapse ,@body)))
     (message "Elapsed time: %.06f" elapsed)))

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
  (message ";; =================== Run-level: %d ========================" run-level))

(defun madmacs--run-level (run-level)
  "Run the configuration for the specified run-level"
  (madmacs--report-run-level run-level)

  (madmacs--measure-time
   (dolist (file (alist-get run-level madmacs--runlevel-files-alist))
     (require file nil (not madmacs-debug)))))

(defun madmacs--boot (&key early )
  "Run the full boot process up until the configured `madmacs-boot-run-level' boot process"
  (message ";; We are all mad here .....")
  (if early
      (progn
        (madmacs--setup-native-compilation)
        (madmacs--run-level 0))
    (progn
      (madmacs--run-level 1)
      (add-hook 'after-init-hook    (lambda () (madmacs--run-level 2)))
      (add-hook 'emacs-startup-hook (lambda () (madmacs--run-level 3))))))

(provide 'madmacs-boot)
