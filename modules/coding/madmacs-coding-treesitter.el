;; -*- lexical-binding: t; -*-

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(ruby go gomod scala rust typescript javascript yaml dockerfile json html css lua shell terraform))
  :config
  (global-treesit-auto-mode))

(provide 'madmacs-coding-treesitter)
