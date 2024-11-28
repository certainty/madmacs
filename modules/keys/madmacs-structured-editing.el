;; -*- lexical-binding: t; -*-

(use-package combobulate
  :ensure t
  :straight (combobulate :type git :host github :repo "mickeynp/combobulate")
  :custom
  (combobulate-key-prefix "C-c o")
  :hook
  (prog-mode . combobulate-mode))

(provide 'madmacs-structured-editing)
