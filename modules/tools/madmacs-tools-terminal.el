;; -*- lexical-binding: t; -*-

(use-package vterm
  :ensure t)

(use-package multi-vterm
  :ensure t
  :after popper
  :bind
  ("C-x T" . multi-vterm-project)
  (:map madmacs-keymap-global
    ("t" . multi-vterm-project))
  

  :config
  (add-to-list 'popper-reference-buffers 'vterm-mode)
  (add-to-list 'display-buffer-alist
    '((lambda (buffer-or-name _)
        (let ((buffer (get-buffer buffer-or-name)))
          (with-current-buffer buffer
            (or (equal major-mode 'vterm-mode)
              (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
       (display-buffer-reuse-window display-buffer-in-side-window)
       (side . right)
       (reusable-frames . visible)
       (window-height . 0.5))))

(provide 'madmacs-tools-terminal)
