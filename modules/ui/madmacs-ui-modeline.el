;; -*- lexical-binding: t; -*-

(use-package moody
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :ensure t
  :config
  (push 'lispyville-mode minions-prominent-modes)
  (minions-mode))

(provide 'madmacs-ui-modeline)
