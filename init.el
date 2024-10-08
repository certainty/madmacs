;; -*- lexical-binding: t; -*-

;; you can still configure madmacs before running the boot process
;; (setopt madmacs-debug t)

(setopt  madmacs-init-features
        '(
          madmacs-keys-essentials
          madmacs-edit-essentials
          madmacs-ux-essentials
          madmacs-ui-windows
          madmacs-ui-frames

          madmacs-ui-theme
          madmacs-ui-faces
          madmacs-ui-fonts

          madmacs-ux-osx
          madmacs-ux-buffers
          ))

(setopt madmacs-after-init-features
        `(
          ,(when (eql madmacs-modal-approach 'evil) 'madmacs-keys-evil)
          ,(when (eql madmacs-modal-approach 'meow) 'madmacs-keys-meow)

          madmacs-ux-completion
          madmacs-ux-dashboard
          madmacs-ui-modeline
          madmacs-ux-repeat

          madmacs-projects-essentials
          madmacs-edit-actions
          madmacs-files-dired
          madmacs-ux-workspaces
          ))

(setopt madmacs-after-startup-features
        '(
          madmacs-coding-treesitter
          ;madmacs-files-treemacs

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
          madmacs-coding-common-lisp
          madmacs-coding-prolog
          madmacs-coding-elixir
          madmacs-coding-elm
          madmacs-coding-graphql
          madmacs-coding-copilot
          madmacs-coding-configurations

          madmacs-tools-docker
          madmacs-tools-calc

          madmacs-org-essentials

          madmacs-games-chess
          madmacs-keys-keybindings
          ))

(madmacs--boot)
