(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t
  :bind
  (:map madmacs-open-map
        ("t" . vterm-toggle)
        ("T" . vterm-toggle-cd))

  :custom
  (vterm-toggle-fullscreen-p nil)

  :config
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . right)
               (reusable-frames . visible)
               (window-height . 0.4))))

(provide 'madmacs-tools-terminal)
