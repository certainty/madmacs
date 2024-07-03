;; -*- lexical-binding: t; -*-

(use-package highlight-numbers
  :ensure t
  :hook prog-mode)

(use-package hl-todo
  :ensure t
  :hook prog-mode)

(use-package goggles
  :ensure t
  :hook (prog-mode text-mode)
  :custom
  (goggles-pulse nil))

(use-package rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t)

(use-package reveal
  :ensure nil
  :straight nil
  :custom
  (reveal-auto-hide nil)
  :config
  (global-reveal-mode))

(use-package emacs
  :ensure nil
  :straight nil
  :custom
  (indicate-empty-lines nil))

(provide 'madmacs-ui-faces)
