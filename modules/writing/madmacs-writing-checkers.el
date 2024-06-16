(use-package flyspell
  :ensure t
  :hook
  (text-mode . flyspell-mode)
  (git-commit-mode . flyspell-mode))

(provide 'madmacs-writing-checkers)
