(use-package emacs
             :ensure nil
             :straight nil
             :demand t
             :config
             ;:after evil-mode
             (message "Setting up leader system")
             (evil-global-set-key 'normal "|" 'split-window-right)
             (evil-global-set-key 'normal "\\" 'split-window-below)
             (evil-global-set-key 'normal (kbd "M-h") 'evil-window-left)
             (evil-global-set-key 'normal (kbd "M-l") 'evil-window-right)
             (evil-global-set-key 'normal (kbd "M-j") 'evil-window-down)
             (evil-global-set-key 'normal (kbd "M-k") 'evil-window-up)

             (defvar-keymap madmacs-leader-keys
               :doc "Everything you need fast under your finger tips")

             (which-key-add-keymap-based-replacements madmacs-leader-keys
               "SPC" `("Avy Char" . avy-goto-char-timer)
               "/" `("Toggle Comment" . comment-dwim)
               "." `("Embark Act" . embark-act)
               "e" `("Neotree" . neotree-toggle)
               "E" `("File Explorer" . dirvish-dwim)
               "q" `("Quit" . kill-emacs)
               "w" `("Save" . evil-save)
               "C" `("Close Buffer" . kill-buffer-and-window)
               "x" `("Execute" . execute-extended-command)
               "R" `("Restart" . restart-emacs)
               "b" `("󰓩  Buffers" . ,madmacs-buffers-keys)
               "D"  `("  Docs" . ,madmacs-docs-keys)
               "d"  `("  Debugger" . ,madmacs-debugger-keys)
               "f" `("  Find" . ,madmacs-find-keys)
               "g" `("󰊢  Git" . ,madmacs-git-keys)
               "l" `("  LSP" . ,madmacs-lsp-keys)
               "m" `("󰑮  Compile" . ,madmacs-compiler-keys)
               "P" `("󰏖  Packages" . ,madmacs-packages-keys)
               "p" `("󱂬  Project" . ,madmacs-project-keys)
               "s" `("󱂬  Session" . ,madmacs-session-keys)
               "t" `(" Terminal" . ,madmacs-terminal-keys)
               "T" `("󰙨 Test" . ,madmacs-test-keys)
               "u" `("  UX" . ,madmacs-ux-keys))

           (evil-define-key 'normal 'global (kbd "<leader>") madmacs-leader-keys))

(provide 'madmacs-keys-leader-system)
