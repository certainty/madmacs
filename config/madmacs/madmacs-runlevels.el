;; absolute bare essentials loaded on early init
(setf (alist-get 0 madmacs--runlevel-files-alist)
      '(madmacs-early-packages
        madmacs-early-ui))

;; set the baseline experience to make the emacs usable and have the basic right
(setf (alist-get 1 madmacs--runlevel-files-alist)
      '(madmacs-init-packages
        madmacs-settings
        madmacs-libraries

        madmacs-ux-essentials
        madmacs-ui-windows
        madmacs-ui-frames

        madmacs-ui-theme
        madmacs-ui-faces
        madmacs-ui-fonts

        madmacs-ux-osx
        madmacs-ux-buffers))

;; everything that enhances the baseline experience
(setf (alist-get 2 madmacs--runlevel-files-alist)
      '(
        madmacs-keys-essentials
        madmacs-keys-meow
        madmacs-ui-modeline
        madmacs-ux-dashboard

                                        ; madmacs-ux-help
                                        ; madmacs-ux-completion
                                        ; madmacs-ux-search
                                        ; madmacs-ui-modeline
                                        ; madmacs-projects
                                        ; madmacs-workspaces-projects
        madmacs-ux-workspaces))

;; all the extra capabilities to program, write, and do other things
(setf (alist-get 3 madmacs--runlevel-files-alist)
      '(
        ; madmacs-keys-leader-system

        ))

(defun madmacs-open-runlevels-file ()
    "Open the runlevels file."
    (interactive)
    (let ((runlevels-file (expand-file-name "madmacs/madmacs-runlevels.el" madmacs--config-dir)))
      (unless (file-exists-p runlevels-file)
        (error "Runlevels file does not exist"))
      (find-file runlevels-file)))

(provide 'madmacs-runlevels)
