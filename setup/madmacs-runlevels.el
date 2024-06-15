(setq madmacs-run-level-1-features
        '(
          madmacs-ux-essentials
          madmacs-ui-windows
          madmacs-ui-frames

          madmacs-ui-theme
          madmacs-ui-faces
          madmacs-ui-fonts

          madmacs-ux-osx
          madmacs-ux-buffers))

(setq madmacs-run-level-2-features
      '(
        madmacs-ux-dashboard
        madmacs-keys-essentials
        madmacs-keys-meow
        madmacs-ui-modeline
        ;; madmacs-ux-help
        ;; madmacs-ux-completion
        ;; madmacs-ux-search
        ;; madmacs-ui-modeline
        ;; madmacs-projects
        ;; madmacs-workspaces-projects
        madmacs-ux-workspaces))

(setq madmacs-run-level-3-features
        '(
          ;; madmacs-keys-leader-system
          ))


(defun madmacs-open-runlevels-file ()
    "Open the runlevels file."
    (interactive)
    (let ((runlevels-file (expand-file-name "madmacs-runlevels.el" madmacs--setup-dir)))
      (unless (file-exists-p runlevels-file)
        (error "Runlevels file does not exist"))
      (find-file runlevels-file)))

(provide 'madmacs-runlevels)
