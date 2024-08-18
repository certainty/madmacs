;; -*- lexical-binding: t; -*-

(use-package ruby-mode
  :ensure nil
  :straight nil
  :mode "\\.\\(?:a?rb\\|aslsx\\)\\'"
  :mode "/\\(?:Brew\\|Fast\\)file\\'"
  :interpreter "j?ruby\\(?:[0-9.]+\\)"
  :hook
  (ruby-ts-mode . madmacs--lsp)
  (ruby-mode . madmacs--lsp)
  :config
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.ruby-lsp\\'"))

  (with-eval-after-load 'treemacs
    (madmacs--treemacs-ignore-files '(".ruby-lsp"))))

(use-package rubocop
  :ensure t
  :hook ruby-ts-mode)

(use-package rspec-mode
  :ensure t
  :mode ("/\\.rspec\\'" . text-mode))

(use-package bundler
  :ensure t)

(use-package inflections
  :ensure t)

(provide 'madmacs-coding-ruby)
