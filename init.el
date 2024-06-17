;; -*- lexical-binding: t; -*-

;; you can still configure madmacs before running the boot process
;; (setopt madmacs-debug t)

(setopt  madmacs-init-features
        '(
          madmacs-keys-essentials
          madmacs-keys-keymaps ; make keymaps available very early on
          madmacs-ux-essentials
          madmacs-ui-windows
          madmacs-ui-frames

          madmacs-ui-theme
          madmacs-ui-faces
          madmacs-ui-fonts

          madmacs-ux-osx
          madmacs-ux-buffers))

(setopt madmacs-after-init-features
        '(
          madmacs-keys-meow
          madmacs-ux-completion
          madmacs-ux-dashboard
          madmacs-ui-modeline

          madmacs-projects-essentials
          madmacs-edit-actions
          madmacs-files-dired
          ;; madmacs-ux-search
          madmacs-ux-workspaces))

(setopt madmacs-after-startup-features
        '(
          madmacs-writing-checkers
          madmacs-writing-essentials
          madmacs-tools-terminal
          madmacs-tools-vcs

          madmacs-coding-essentials
          madmacs-coding-version-manager
          madmacs-coding-checkers
          madmacs-coding-lsp
          madmacs-coding-dap
          madmacs-coding-scala
          madmacs-coding-ruby
          madmacs-coding-golang
          madmacs-coding-typescript
          madmacs-coding-web
          madmacs-coding-terraform
          madmacs-coding-lisp
          madmacs-coding-common-lisp

          madmacs-coding-copilot

;; FIXME: doesn't load yet
          ;madmacs-org-essentials
          ))

(madmacs--boot)
