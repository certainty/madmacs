;; (use-package ruby-mode
;;   :ensure t
;;   :mode "\\.\\(?:a?rb\\|aslsx\\)\\'"
;;   :mode "/\\(?:Brew\\|Fast\\)file\\'"
;;   :interpreter "j?ruby\\(?:[0-9.]+\\)"
;;   :hook (ruby-mode . lsp-deferred)
;;   :custom
;;   (ruby-insert-encoding-magic-comment nil)
;;   :config
;;   (with-eval-after-load 'lsp-mode
;;     (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.ruby-lsp\\'"))
;;   )

(use-package ruby-ts-mode
  :ensure nil
  :straight nil
  :mode "\\.\\(?:a?rb\\|aslsx\\)\\'"
  :mode "/\\(?:Brew\\|Fast\\)file\\'"
  :interpreter "j?ruby\\(?:[0-9.]+\\)"
  :hook (ruby-ts-mode . lsp-deferred)
  :config
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.ruby-lsp\\'")))

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
