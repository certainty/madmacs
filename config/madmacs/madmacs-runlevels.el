
(setf (alist-get 0 madmacs--runlevel-files-alist)
      '(madmacs-early-packages
        madmacs-early-ui
        ))

(setf (alist-get 1 madmacs--runlevel-files-alist)
      '())

(setf (alist-get 2 madmacs--runlevel-files-alist)
      '())

(setf (alist-get 3 madmacs--runlevel-files-alist)
      '())

(provide 'madmacs-runlevels)
