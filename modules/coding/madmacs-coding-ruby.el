(use-package ruby-mode
  :ensure t
  :mode "\\.\\(?:a?rb\\|aslsx\\)\\'"
  :mode "/\\(?:Brew\\|Fast\\)file\\'"
  :interpreter "j?ruby\\(?:[0-9.]+\\)"
  :hook (ruby-mode . lsp-deferred)
  :custom
  (ruby-insert-encoding-magic-comment nil))

(use-package rubocop
  :ensure t
  :hook ruby-mode)

(use-package rspec-mode
  :ensure t
  :mode ("/\\.rspec\\'" . text-mode))

(use-package bundler
  :ensure t)

(use-package inflections
  :ensure t)

(provide 'madmacs-coding-ruby)
