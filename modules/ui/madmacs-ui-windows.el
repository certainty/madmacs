;; -*- lexical-binding: t; -*-

(use-package window
  :straight nil
  :ensure nil
  :custom
  (display-buffer-base-action nil))

(use-package ace-window
  :ensure t
  :demand t
  :bind
  ("M-o" . ace-window))

(use-package windmove
  :straight nil
  :ensure nil
  :config
  (windmove-default-keybindings))

(use-package winner
  :straight nil
  :ensure nil
  :config
  (winner-mode 1))

(use-package transpose-frame
  :ensure t)

(use-package posframe
  :ensure t)

(provide 'madmacs-ui-windows)
