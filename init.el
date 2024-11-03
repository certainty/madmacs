;; -*- lexical-binding: t; -*-

;; (setopt madmacs-debug t)

(setopt  madmacs-init-features
  `(
     madmacs-edit-essentials
     madmacs-ux-essentials
     madmacs-ui-windows
     madmacs-ui-frames

     madmacs-ui-theme
     madmacs-ui-faces
     madmacs-ui-fonts

     madmacs-ux-osx
     madmacs-ux-buffers
     madmacs-keys-meow
     madmacs-keys-essentials            ;; it is important to load this after meow, to make sure the leader map works correctly
     ))

(setopt madmacs-after-init-features
  '(
     madmacs-ux-completion
     madmacs-ux-dashboard
     madmacs-ui-modeline
     madmacs-ux-repeat

     madmacs-projects-essentials
     madmacs-edit-actions
     madmacs-files-dired
     madmacs-ux-workspaces))

(setopt madmacs-after-startup-features
  '(
     madmacs-coding-treesitter
     madmacs-writing-checkers
     madmacs-writing-essentials

     madmacs-tools-terminal
     madmacs-tools-vcs

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
     madmacs-coding-lisp
     madmacs-coding-elisp
     madmacs-coding-common-lisp
     madmacs-coding-prolog
     madmacs-coding-elixir
     madmacs-coding-elm
     madmacs-coding-graphql
     madmacs-coding-copilot
     madmacs-coding-configurations

     madmacs-tools-docker
     madmacs-tools-calc
     madmacs-tools-pass
     madmacs-tools-ai
     madmacs-tools-translate

     madmacs-org-essentials

     madmacs-games-chess

     madmacs-keys-keybindings))

(madmacs--boot)
