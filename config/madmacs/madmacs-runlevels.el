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

(provide 'madmacs-runlevels)
