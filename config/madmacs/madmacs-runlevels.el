
(setf (alist-get 0 madmacs--runlevel-files-alist)
      '(madmacs-early-packages
        madmacs-early-ui))

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
        madmacs-ux-buffers
        ))

(setf (alist-get 2 madmacs--runlevel-files-alist)
      '(madmacs-ux-workspaces))

(setf (alist-get 3 madmacs--runlevel-files-alist)
      '())

(provide 'madmacs-runlevels)
