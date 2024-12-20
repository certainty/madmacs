;; -*- lexical-binding: t; -*-

(use-package asdf
  :straight (asdf :type git :host github :repo "tabfugnic/asdf.el")
  :hook (after-init . asdf-enable))

(use-package envrc
  :hook (after-init . envrc-global-mode))

;; Elixir / Erlang
(use-package inf-elixir)

(use-package apprentice
  :straight (apprentice :type git :host github :repo "Sasanidas/Apprentice")
  :hook (elixir-mode . apprentice-mode)
  :custom
  (appretice-key-command-prefix "C-c z"))

(use-package elixir-mode
  :hook
  (elixir-mode . apprentice-mode)
  (elixir-mode . eglot-ensure)
  :init
  (add-to-list 'exec-path ' "/opt/homebrew/bin/elixir-ls"))

;; elm
(use-package elm-mode)

;; golang
(use-package go-mode
  :straight nil
  :mode "\\.go\\'"
  :hook
  (go-ts-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  :config
  (setopt gofmt-command "goimports")
  (setq-default indent-tabs-mode nil))

(use-package go-mod-mode
  :straight nil
  :mode "\\.mod\\'"
  :hook
  (go-mod-ts-mode . eglot-ensure)
  (go-mod-mode . eglot-ensure))

(use-package go-guru)

;;; Ruby

(use-package ruby-mode
  :straight nil
  :mode "\\.\\(?:a?rb\\|aslsx\\)\\'"
  :mode "/\\(?:Brew\\|Fast\\)file\\'"
  :interpreter "j?ruby\\(?:[0-9.]+\\)"
  :hook
  (ruby-ts-mode . eglot-ensure)
  (ruby-mode . eglot-ensure))

(use-package rubocop
  :hook ruby-ts-mode)

(use-package rspec-mode
  :mode ("/\\.rspec\\'" . text-mode))

(use-package bundler)
(use-package inflections)

;;; Typescript
(use-package jest-test-mode)

(use-package typescript-mode
  :after jest
  :hook
  (typescript-mode . jest-test-mode)
  (typescript-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (typescript-ts-mode . jest-test-mode)
  (js-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure))

(use-package prettier-js
  :hook (typescript-mode typescrip-ts-mode))

;;; Graphql

(use-package graphql-ts-mode
  :mode ("\\.graphql\\'" "\\.gql\\'"))

(use-package terraform-mode
  :hook eglot-ensure)

;;; Web stuff
(use-package web-mode
  :hook (web-mode . eglot-ensure))

(use-package css-mode
  :hook
  (css-mode . eglot-ensure)
  (css-ts-mode . eglot-ensure))


;;; Configurations
(use-package yaml)

(use-package yaml-mode
  :straight nil
  :mode "\\.ya?ml\\'"
  :hook
  (yaml-mode  . eglot-ensure))

(use-package json-mode
  :mode "\\.json\\'"
  :hook
  (json-mode . eglot-ensure))

(use-package csv-mode
  :hook (csv-mode . csv-guess-set-separator)
  :mode "\\.csv\\'")

(provide 'madmacs-code-misc)
