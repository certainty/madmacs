;; -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :straight nil
  :demand t
  :config
  
  ;; Here I define the global keys and leader keys.
  ;; I put them in one place to find / correct / avoid conflicts easier.
  ;; For major modes I may still define the localleader bindings in the packages themselves,
  ;; since they are local and the risk of conflicts is way smaller.
  ;;
  ;; I am careful not to occupy the local leader key here
  
  (evil-global-set-key 'normal "|" 'split-window-right)
  (evil-global-set-key 'normal "\\" 'split-window-below)
  (evil-global-set-key 'normal (kbd "M-h") 'evil-window-left)
  (evil-global-set-key 'normal (kbd "M-l") 'evil-window-right)
  (evil-global-set-key 'normal (kbd "M-j") 'evil-window-down)
  (evil-global-set-key 'normal (kbd "M-k") 'evil-window-up)

  (evil-global-set-key 'visual (kbd "v") 'er/expand-region)
  (evil-global-set-key 'visual (kbd "V") 'er/contract-region)

  ;; Buffers
  (defvar-keymap madmacs-buffers-keys :doc "Buffer related commands and utilities")
  
  (which-key-add-keymap-based-replacements madmacs-buffers-keys
    "=" '("Balance" . balance-windows)
    "v" '("Split vertical" . split-window-right)
    "H" '("Split horizontal" . split-window-below)
    "o" '("Other" . other-window)
    "b" '("ACE"  . ace-window)
    "d" '("Close" . delete-window)
    "D" '("Close all others" . delete-other-windows)
    "x" '("Swap" . ace-swap-window)
    "m" '("Maximize" . ace-maximize-window))

  ;; Compiler / Build / Quickrun
  (defvar-keymap madmacs-compiler-keys :doc "Keys to build projects and interact with the compiler")

  ;; Docs
  (defvar-keymap madmacs-docs-keys :doc "Keys to find help and documentation")
  
  (which-key-add-keymap-based-replacements madmacs-docs-keys
    "m" '("Mode commands" . consult-mode-command)
    "M" '("Describe mode" . describe-mode)
    "." '("Help at point" . helpful-at-point)
    "k" '("Describe Key" . describe-key)
    "K" '("Describe Keymap" . describe-keymap))
  
  ;; Find
  (defvar-keymap madmacs-find-keys :doc "Keys which are related to finding and navigating things")
  
  (which-key-add-keymap-based-replacements madmacs-find-keys
    "c" '("Find Char" . avy-goto-char-2)
    "f" '("Find Project File" . project-find-file)
    "." '("Find Project Things" . constult-project-extra-find)
    "b" '("Find Project Buffer" . consult-project-buffer)
    "B" '("Find Buffer" .  consult-buffer)
    "i" `("Find Imenu" . consult-imenu)
    "g" '("Find Word in Project" . consult-ripgrep)
    "L" '("Find and goto line" .  consult-goto-line)
    "l" '("Find line" . consult-line)
    "o" '("Find recent file" . consult-recent-file)
    "m" '("Find bookmark" . consult-bookmark)
    "a" '("Find apropos" .  consult-apropos)
    "R" '("Find and replace" . query-replace)
    "R" '("Find and replace in project" . project-query-replace-regexp)
    "y" '("Find yank" .  consult-yank-pop)
    "p" '("Find project and session" . tabspaces-open-or-create-project-and-workspace)
    "P" '("Find Project" . project-switch-project))

  ;; Git
  (defvar-keymap madmacs-git-keys :doc "Keys to interact with git")
  (which-key-add-keymap-based-replacements madmacs-git-keys
    "," '("Dispatch" . magit-dispatch)
    "." '("File dispatch" . magit-file-dispatch)
    "b" '("Blame" . magit-blame)
    "g" '("Status" . magit-status)
    "l" '("Log" . magit-log)
    "t" '("Toggle timemachine" . git-timemachine-toggle))
  
  ;; LSP 
  (defvar-keymap madmacs-lsp-keys :doc "LSP keys that are the same for all languages")

  (defvar-keymap madmacs-lsp-workspace-keys :doc "LSP prefix map")

  (when (eq madmacs-lsp-client 'lsp-mode)
    (which-key-add-keymap-based-replacements madmacs-lsp-workspace-keys
      "D" `("Disconnect" .  lsp-disconnect)
      "d" `("Describe" . lsp-describe-session)
      "q" `("Shutdown Server" . lsp-workspace-shutdown)
      "r" `("Restart Server" .  lsp-workspace-restart)
      "s" `("Start Server" . lsp)))
  
  (when (eq madmacs-lsp-client 'eglot)
    (which-key-add-keymap-based-replacements madmacs-lsp-workspace-keys
      "q" `("Shutdown Server" . eglot-shutdown)
      "Q" `("Shutdown All" . eglot-shutdown-all)
      "r" `("Reconnect Server" . eglot-connect)
      "u" `("Update Server" . eglot-upgrade-eglot)
      "s" `("Start Server" . eglot)
      "c" `("Show configuration" . eglot-show-workspace-configuration)))

  (defvar-keymap madmacs-lsp-folders-keys :doc "LSP prefix map")

  (when (eq madmacs-lsp-client 'lsp-mode)
    (which-key-add-keymap-based-replacements madmacs-lsp-folders-keys
      "a" `("Add" . lsp-workspace-folders-add)
      "b" `("un-blocklist folder" . lsp-workspace-blocklist-remove)
      "r" `("remove folder" . lsp-workspace-folders-remove)))

  (defvar-keymap madmacs-lsp-formatting-keys :doc "LSP prefix map")

  (when (eq madmacs-lsp-client 'lsp-mode)
    (which-key-add-keymap-based-replacements madmacs-lsp-formatting-keys
      "b" `("Buffer" . lsp-format-buffer)
      "r" `("Region" . lsp-format-region)))

  (when (eq madmacs-lsp-client 'eglot)
    (which-key-add-keymap-based-replacements madmacs-lsp-formatting-keys
      "b" `("Buffer" . eglot-format-buffer)
      "f" `("Region" . eglot-format)))

  (defvar-keymap madmacs-lsp-toggle-keys :doc "LSP prefix map")

  (when (eq madmacs-lsp-client 'lsp-mode)
    (which-key-add-keymap-based-replacements madmacs-lsp-toggle-keys
      "D" `("Toggle modeline diagnostics" . lsp-modeline-diagnostics-mode)
      "L" `("Toggle log io" . lsp-toggle-trace-io)
      "l" `("Toggle lenses" . lsp-lens-mode)))

  (when (eq madmacs-lsp-client 'eglot)
    (which-key-add-keymap-based-replacements madmacs-lsp-toggle-keys
      "h" `("Toggle inlay hinds" . eglot-inlay-hints-mode)))

  (defvar-keymap madmacs-lsp-goto-keys :doc "LSP prefix map")
  (when (eq madmacs-lsp-client 'lsp-mode)
    (which-key-add-keymap-based-replacements madmacs-lsp-goto-keys
      "a" `("Find symbol in workspace" . xref-find-apropos)
      "D" `("Find declaration" . lsp-find-declaration)
      "d" `("Find definition" . lsp-find-definition)
      "i" `("Find implementation" . lsp-find-implementation)
      "r" `("Find references" . lsp-find-references)
      "t" `("Find type definition" . lsp-find-type-definition)))

  (when (eq madmacs-lsp-client 'eglot)
    (which-key-add-keymap-based-replacements madmacs-lsp-goto-keys
      "a" `("Find symbol in workspace" . xref-find-apropos)
      "D" `("Find declaration" . eglot-find-declaration)
      "d" `("Find definition" . xref-find-definitions)
      "i" `("Find implementation" . eglot-find-implementation)
      "r" `("Find references" . xref-find-references)
      "t" `("Find type definition" . eglot-find-typeDefinition)))

  (defvar-keymap madmacs-lsp-help-keys :doc "LSP prefix map")
  (when (eq madmacs-lsp-client 'lsp-mode)
    (which-key-add-keymap-based-replacements madmacs-lsp-help-keys
      "g" `("Glance Symbol" . lsp-ui-doc-glance)
      "." `("Describe thing" . lsp-describe-thing-at-point)
      "s" `("Signature" . lsp-signature-activate)))

  (when (eq madmacs-lsp-client 'eglot)
    (which-key-add-keymap-based-replacements madmacs-lsp-help-keys
      "d" `("Describe thing" . helpful-at-point)
      "." `("Help for thing" . display-local-help)))

  (defvar-keymap madmacs-lsp-refactor-keys :doc "LSP prefix map")
  (when (eq madmacs-lsp-client 'lsp-mode)
    (which-key-add-keymap-based-replacements madmacs-lsp-refactor-keys
      "o" `("Optimize Imports" . lsp-organize-imports)
      "r" `("Rename" . lsp-renam)))

  (when (eq madmacs-lsp-client 'eglot)
    (which-key-add-keymap-based-replacements madmacs-lsp-refactor-keys
      "o" `("Optimize Imports" . eglot-code-action-organize-imports)
      "r" `("Rename" . eglot-rename)
      "i" `("Inline" . eglot-code-action-inline)
      "e" `("Extract" . eglot-code-action-extract)))

  (defvar-keymap madmacs-lsp-actions-keys :doc "LSP prefix map")
  (when (eq madmacs-lsp-client 'lsp-mode)
    (which-key-add-keymap-based-replacements madmacs-lsp-actions-keys
      "a" `("Execute" . lsp-execute-code-action)
      "h" `("Highlight symbol" . lsp-document-highlight)
      "l" `("Avy lens" . lsp-avy-lens)))

  (when (eq madmacs-lsp-client 'eglot)
    (which-key-add-keymap-based-replacements madmacs-lsp-actions-keys
      "a" `("Execute" . eglot-code-actions)
      "f" `("Fix" . eglot-code-action-quickfix)))

  (defvar-keymap madmacs-lsp-peek-keys :doc "LSP prefix map")
  (when (eq madmacs-lsp-client 'lsp-mode)
    (which-key-add-keymap-based-replacements madmacs-lsp-peek-keys
      "d" `("Peek definitions" . lsp-ui-peek-find-definitions)
      "i" `("Peek implementations" . lsp-ui-peek-find-implementation)
      "r" `("Peek references" . lsp-ui-peek-find-references)
      "s" `("Peek workspace symbol" . lsp-ui-peek-find-workspace-symbol)))


  (when (eq madmacs-lsp-client 'lsp-mode)
    (which-key-add-keymap-based-replacements madmacs-lsp-keys
      "!"  '("Diagnostics" . consult-lsp-diagnostics)
      "a" `("Code actions" . ,madmacs-lsp-actions-keys)
      "f" `("Formatting" . ,madmacs-lsp-formatting-keys)
      "F" `("Folders" . ,madmacs-lsp-folders-keys)
      "g" `("Goto" . ,madmacs-lsp-goto-keys)
      "G" `("Peek" . ,madmacs-lsp-peek-keys)
      "h" `("Help" . ,madmacs-lsp-help-keys)
      "l" '("IMenu" . consult-imenu)
      "r" `("Refactor" . ,madmacs-lsp-refactor-keys)
      "S"  '("Symbols" . consult-lsp-file-symbols)
      "T" `("Toggle" . ,madmacs-lsp-toggle-keys)
      "w" `("Workspaces" . ,madmacs-lsp-workspace-keys)
      ))

  (when (eq madmacs-lsp-client 'eglot)
    (which-key-add-keymap-based-replacements madmacs-lsp-keys
      "!" '("Diagnostics" . flymake-show-buffer-diagnostics)
      "a" `("Code actions" . ,madmacs-lsp-actions-keys)
      "f" `("Formatting" . ,madmacs-lsp-formatting-keys)
      "g" `("Goto" . ,madmacs-lsp-goto-keys)
      "h" `("Help" . ,madmacs-lsp-help-keys)
      "l" '("IMenu" . consult-imenu)
      "r" `("Refactor" . ,madmacs-lsp-refactor-keys)
      "T" `("Toggle" . ,madmacs-lsp-toggle-keys)
      "w" `("Workspaces" . ,madmacs-lsp-workspace-keys)))

  ;; Debugger
  (defvar-keymap madmacs-debugger-keys :doc "Keys related to debugging")
  
  (which-key-add-keymap-based-replacements madmacs-debugger-keys
    "d" '("Dap hydra" . dap-hydra))


  ;; Project
  (defvar-keymap madmacs-project-keys :doc "Keys related to interactions with the current project")

  (which-key-add-keymap-based-replacements madmacs-project-keys
    "r"   '("Query replace" . project-query-replace-regexp)
    "d"   '("Consult dir" . consult-dir)
    "R"   '("Remember project under" . project-remember-projects-under)
    "q"   '("Kill buffers" . project-kill-buffers))

  (defvar-keymap madmacs-packages-keys :doc "Keys to interact with the package manager and the configuration")
  (which-key-add-keymap-based-replacements madmacs-packages-keys
    "u" '("Pull / Update package" . straight-pull-package)
    "U" '("Pull / Update all" . straight-pull-all))

  ;; Sessions
  (defvar-keymap madmacs-session-keys :doc "Keys to interact with the session / workspace")
  (which-key-add-keymap-based-replacements madmacs-session-keys
    "o" '("Open or create session" . tabspaces-open-or-create-project-and-workspace)
    "q" '("Close session" . tabspaces-close-workspace))

  ;; Terminal
  (defvar-keymap madmacs-terminal-keys :doc "Keys related to interact with the terminal")
  
  (which-key-add-keymap-based-replacements madmacs-terminal-keys
    "t" '("Toggle Term" . vterm-toggle)
    "T" '("Toggle Term" . vterm-toggle-cd))

  ;; UX
  (defvar-keymap madmacs-ux-keys :doc "Keys to control and toggle various aspects of the ux")
  
  (which-key-add-keymap-based-replacements madmacs-ux-keys
    "f" '("Toggle Big Font" . madmacs-toggle-font)
    "g" '("Toggle golden ratio" . golden-ratio-mode)
    "G" '("Toggle golden ratio widescreen" . golden-ratio-toggle-widescreen)
    "l" '("Line numbers" . display-line-numbers-mode)
    "t" '("Highlight Todo" . hl-todo-mode)
    "h" '("Highlight Line" . hl-line-mode)
    "H" '("Global Highlight Line" . global-hl-line-mode)
    "W" '("Toggle Fullscreen" . toggle-frame-fullscreen)
    )

  ;; Madmacs
  (defvar-keymap madmacs-madmacs-keys :doc "Keys for madmacs related functionality")
  (which-key-add-keymap-based-replacements madmacs-madmacs-keys
    "r" '("Restart" . restart-emacs)
    "q" '("Quit" . kill-emacs))

  (defvar-keymap madmacs-checker-keys :doc "Keys for checking flycheck / flymake")
  (which-key-add-keymap-based-replacements madmacs-checker-keys
    "v" '("Verify" . flycheck-verify-setup)
    "." '("Explain" . flycheck-explain-error-at-point)
    "," '("Help" . flycheck-display-error-at-point)
    "l" '("List" . flycheck-list-errors)
    "C" '("Clear" . flycheck-clear))

  (defvar-keymap madmacs-leader-keys :doc "Everything you need fast under your finger tips")

  (which-key-add-keymap-based-replacements madmacs-leader-keys
    "!" `("Checkers" . ,madmacs-checker-keys)
    "/" `("Toggle Comment" . comment-dwim)
    "." `("Embark Act" . embark-act)
    "," `("Embark Dwim" . embark-dwim)
    "'" '("Iedit" . iedit-mode)
    "\"" '("Iedit Dwim" . iedit-dwim)
    "SPC" `("Avy" . avy-goto-char-timer)
    "x" `("M-x" . execute-extended-command)
    "e" `("Filetree" . treemacs)
    "E" `("Filetree Dwim" . dirvish-dwim)
    "b" `("󰓩  Buffers" . ,madmacs-buffers-keys)
    "D"  `("  Docs" . ,madmacs-docs-keys)
    "d"  `("  Debugger" . ,madmacs-debugger-keys)
    "f" `("  Find" . ,madmacs-find-keys)
    "g" `("󰊢  Git" . ,madmacs-git-keys)
    "l" `("  LSP" . ,madmacs-lsp-keys)
    "m" `("󰑮  Compile" . ,madmacs-compiler-keys)
    "M" `("  Madmacs" . ,madmacs-madmacs-keys)
    "P" `("󰏖  Packages" . ,madmacs-packages-keys)
    "p" `("󱂬  Project" . ,madmacs-project-keys)
    "s" `("󱂬  Session" . ,madmacs-session-keys)
    "t" `(" Terminal" . ,madmacs-terminal-keys)
    "u" '("󰁕 Universal Argument" . universal-argument) 
    "U" `("  UX" . ,madmacs-ux-keys))

  (evil-define-key 'normal 'global (kbd "<leader>") madmacs-leader-keys)
  )

(provide 'madmacs-keys-keybindings)
