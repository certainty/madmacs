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
          ;madmacs-keys-leader-system
          ))



(provide 'madmacs-runlevels)
