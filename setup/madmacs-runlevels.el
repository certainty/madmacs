(setq madmacs-run-level-1-features
        '(
          madmacs-keys-essentials
          madmacs-keys-keymaps ; make keymaps available very early on
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
          ;madmacs-keys-leader-system
          ))


(defun madmacs-open-runlevels-file ()
    "Open the runlevels file."
    (interactive)
    (let ((runlevels-file (expand-file-name "madmacs-runlevels.el" madmacs--setup-dir)))
      (unless (file-exists-p runlevels-file)
        (error "Runlevels file does not exist"))
      (find-file runlevels-file)))

(add-hook 'emacs-startup-hook (lambda ()
                                (bind-keys :map madmacs-madmacs-map ("r" . madmacs-open-runlevels-file))
                                (madmacs--describe-key-in-keymap madmacs-madmacs-map "r" "Go to runlevels.el")))

(provide 'madmacs-runlevels)
