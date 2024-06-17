

;; Elixir / Erlang
(use-package inf-elixir
  :ensure t)

(use-package apprentice
  :ensure t
  :straight (apprentice :type git :host github :repo "Sasanidas/Apprentice")
  :hook (elixir-mode . apprentice-mode)
  :custom
  (apprentice-key-command-prefix "C-c l"))

(use-package elixir-mode
  :ensure t
  :hook
  (elixir-mode . apprentice-mode)
  (elixir-mode . lsp-deferred)
  :init
  (add-to-list 'exec-path ' "/opt/homebrew/bin/elixir-ls"))


(provide 'madmacs-coding-elixir)
