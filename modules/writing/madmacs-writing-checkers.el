;; -*- lexical-binding: t; -*-

(use-package flyspell
  :ensure t
  :hook
  (text-mode . flyspell-mode)
  (git-commit-mode . flyspell-mode)
  :config
  ;; don't shadow embark
  (unbind-key "C-." flyspell-mode-map))

(provide 'madmacs-writing-checkers)
