;; -*- lexical-binding: t; -*-

;; (setopt madmacs-debug t)

(setopt  madmacs-init-features
  `(
     madmacs-ux-minor-mode
     madmacs-ux-display-elements
     madmacs-ui-look
     madmacs-ui-modeline
     madmacs-edit-essentials
     madmacs-keys-essentials            ;; it is important to load this after meow, to make sure the leader map works correctly and is correctly displayed
     madmacs-keys-emacs
    ))

(setopt madmacs-after-init-features
  '(
     madmacs-ux-completion
     madmacs-ux-dashboard
     madmacs-ux-repeat

     madmacs-edit-actions

     madmacs-files-essentials
     madmacs-files-dired

     madmacs-ux-workspaces
     ))

(setopt madmacs-after-startup-features
  '(
     madmacs-coding-treesitter
     madmacs-writing-checkers
     madmacs-writing-essentials

     madmacs-tools-terminal
     madmacs-tools-vcs

     ;; code support
     madmacs-coding-essentials

     madmacs-coding-version-manager
     madmacs-coding-checkers
     madmacs-coding-lsp
     madmacs-coding-debug
     madmacs-coding-scala
     madmacs-coding-ruby
     madmacs-coding-golang
     madmacs-coding-typescript
     madmacs-coding-web
     madmacs-coding-terraform
     madmacs-coding-elisp
     madmacs-coding-common-lisp
     madmacs-coding-racket
     madmacs-coding-prolog
     madmacs-coding-elixir
     madmacs-coding-elm
     madmacs-coding-graphql
     madmacs-coding-copilot
     madmacs-coding-configurations

     ;; useful tools
     madmacs-tools-docker
     madmacs-tools-calc
     madmacs-tools-pass
     madmacs-tools-ai
     madmacs-tools-translate

     ;; add-ons
     madmacs-org-essentials

     madmacs-games-chess

     ;; optional consistent keybindings
     madmacs-keys-keybindings
     ))

(madmacs--boot)
