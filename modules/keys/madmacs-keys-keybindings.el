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
  
  (defvar-keymap madmacs-windows-keys :doc "Window related commands and utilities")
  
  (which-key-add-keymap-based-replacements madmacs-windows-keys
    "=" '("Balance" . balance-windows)
    "v" '("Split vertical" . split-window-right)
    "h" '("Split horizontal" . split-window-below)
    "o" '("Other" . other-window)
    "w" '("ACE"  . ace-window)
    "d" '("Close" . delete-window)
    "D" '("Close all others" . delete-other-windows)
    "x" '("Swap" . ace-swap-window)
    "m" '("Maximize" . ace-maximize-window)
    "f" '("Flip" . flip-frame)
    "F" '("Flop" . flop-frame)
    "r" '("Rotate" . rotate-frame-clockwise)
    "t" '("Transpose" . transpose-frame))

  ;; Compiler / Build / Quickrun
  (defvar-keymap madmacs-compiler-keys :doc "Keys to build projects and interact with the compiler")
  (which-key-add-keymap-based-replacements madmacs-compiler-keys
    "." '("Quickrun" . quickrun)
    "," '("Quickrun region" . quickrun-region)
    ":" '("Quickrun select" . quickrun-select)
    "r" '("Project run" . project-compile)
    "R" '("Project rerun" . project-recompile)
    "c" '("Run" . compile)
    "C" '("Rerun" . recompile))

  ;; Docs
  (defvar-keymap madmacs-docs-keys :doc "Keys to find help and documentation")
  
  (which-key-add-keymap-based-replacements madmacs-docs-keys
    "m" '("Mode commands" . consult-mode-command)
    "M" '("Describe mode" . describe-mode)
    "." '("Help at point" . helpful-at-point)
    "k" '("Describe Key" . describe-key)
    "K" '("Describe Keymap" . describe-keymap))
  
  ;; Goto
  (which-key-add-keymap-based-replacements goto-map
    "f" '("Project File" . project-find-file)
    "." '("Project Things" . consult-project-extra-find)
    "," '("Global Mark" . consult-global-mark)
    "c" '("Char" . avy-goto-char)
    "d" '("Filetree" . dirvish-side)
    "e" '("Isearch Hist" . consult-isearch-history)
    "E" '("Error" . consult-compile-error)
    "!" '("Check" . consult-flymake)
    "b" '("Project Buffer" . consult-project-buffer)
    "B" '("Buffer" .  consult-buffer)
    "i" `("Imenu" . consult-imenu)
    "I" '("Imenu multi" . consult-imenu-multi)
    "g" '("Line" . consult-goto-line)
    "r" '("Recent file" . consult-recent-file)
    "m" '("Mark" . consult-mark)
    "k" '("Global Mark" . consult-global-mark)
    "a" '("Find apropos" .  consult-apropos)
    "o" '("Outline" . consult-outline)
    "p" '("Find project and session" . tabspaces-open-or-create-project-and-workspace)
    "P" '("Find Project" . project-switch-project)
    "w" '("Window" . ace-window)
    "t" '("Terminal" . vterm-toggle)
    "#" '("Register" . consult-register))

  ;; search
  (which-key-add-keymap-based-replacements search-map
    "p" '("Ripgrep" . consult-ripgrep)
    "l" '("Line" . consult-line)
    "g" '("Grep" . consult-grep)
    "G" '("Git grep" . consult-git-grep)
    "u" '("Focus line" . consult-focus-lines)
    "R" '("Find and replace in Project" . project-query-replace-regexp))

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
  (which-key-add-keymap-based-replacements madmacs-lsp-workspace-keys
    "q" `("Shutdown Server" . eglot-shutdown)
    "Q" `("Shutdown All" . eglot-shutdown-all)
    "r" `("Reconnect Server" . eglot-connect)
    "u" `("Update Server" . eglot-upgrade-eglot)
    "s" `("Start Server" . eglot)
    "c" `("Show configuration" . eglot-show-workspace-configuration))
  

  (defvar-keymap madmacs-lsp-formatting-keys :doc "LSP prefix map")
  (which-key-add-keymap-based-replacements madmacs-lsp-formatting-keys
    "b" `("Buffer" . eglot-format-buffer)
    "f" `("Region" . eglot-format))


  (defvar-keymap madmacs-lsp-toggle-keys :doc "LSP prefix map")
  (which-key-add-keymap-based-replacements madmacs-lsp-toggle-keys
    "h" `("Toggle inlay hinds" . eglot-inlay-hints-mode))

  (defvar-keymap madmacs-lsp-goto-keys :doc "LSP prefix map")
  (which-key-add-keymap-based-replacements madmacs-lsp-goto-keys
    "a" `("Find symbol in workspace" . xref-find-apropos)
    "D" `("Find declaration" . eglot-find-declaration)
    "d" `("Find definition" . xref-find-definitions)
    "i" `("Find implementation" . eglot-find-implementation)
    "r" `("Find references" . xref-find-references)
    "t" `("Find type definition" . eglot-find-typeDefinition))
  

  (defvar-keymap madmacs-lsp-help-keys :doc "LSP prefix map")
  (which-key-add-keymap-based-replacements madmacs-lsp-help-keys
    "d" `("Describe thing" . helpful-at-point)
    "." `("Help for thing" . display-local-help))
  

  (defvar-keymap madmacs-lsp-refactor-keys :doc "LSP prefix map")
  (which-key-add-keymap-based-replacements madmacs-lsp-refactor-keys
    "o" `("Optimize Imports" . eglot-code-action-organize-imports)
    "r" `("Rename" . eglot-rename)
    "i" `("Inline" . eglot-code-action-inline)
    "e" `("Extract" . eglot-code-action-extract))
  

  (defvar-keymap madmacs-lsp-actions-keys :doc "LSP prefix map")
  (which-key-add-keymap-based-replacements madmacs-lsp-actions-keys
    "a" `("Execute" . eglot-code-actions)
    "f" `("Fix" . eglot-code-action-quickfix))
  
  
  (which-key-add-keymap-based-replacements madmacs-lsp-keys
    "!" '("Diagnostics" . flymake-show-buffer-diagnostics)
    "a" `("Code actions" . ,madmacs-lsp-actions-keys)
    "f" `("Formatting" . ,madmacs-lsp-formatting-keys)
    "g" `("Goto" . ,madmacs-lsp-goto-keys)
    "h" `("Help" . ,madmacs-lsp-help-keys)
    "l" '("IMenu" . consult-imenu)
    "r" `("Refactor" . ,madmacs-lsp-refactor-keys)
    "T" `("Toggle" . ,madmacs-lsp-toggle-keys)
    "w" `("Workspaces" . ,madmacs-lsp-workspace-keys))
  

  ;; Debugger
  (defvar-keymap madmacs-debugger-keys :doc "Keys related to debugging")
  
  (which-key-add-keymap-based-replacements madmacs-debugger-keys
    "d" '("Dap hydra" . dap-hydra))

  ;; Project
  (defvar-keymap madmacs-project-keys :doc "Keys related to interactions with the current project")

  (which-key-add-keymap-based-replacements madmacs-project-keys
    "a"   '("GPTel" . madmacs-gptel-project)
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
    "c" '("Toggle Color Scheme (light/dark)" . madmacs-modus-theme-toggle)
    "f" '("Toggle Big Font" . madmacs-toggle-font)
    "g" '("Toggle golden ratio" . golden-ratio-mode)
    "G" '("Toggle golden ratio widescreen" . golden-ratio-toggle-widescreen)
    "l" '("Line numbers" . display-line-numbers-mode)
    "t" '("Highlight Todo" . hl-todo-mode)
    "h" '("Highlight Line" . global-hl-line-mode)
    "H" '("Global Highlight Line" . global-hl-line-mode)
    "W" '("Toggle Fullscreen" . toggle-frame-fullscreen))

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
    "n" '("Next" . flycheck-next-error)
    "p" '("Previous" . flycheck-previous-error)
    "l" '("List" . flycheck-list-errors)
    "C" '("Clear" . flycheck-clear))

  (defvar-keymap madmacs-list-keys :doc "List various entities")
  (which-key-add-keymap-based-replacements madmacs-list-keys
    "b" '("Buffers" . ibuffer)
    "m" '("Bookmarks" . list-bookmarks))


  (defvar-keymap madmacs-gptel-keys :doc "Keys for AI related functionality")
  (which-key-add-keymap-based-replacements madmacs-gptel-keys
    "." '("Quick" . gptel-quick)
    "?" '("Ask" . gptel-ask)
    "," '("Complete at point" . gptel-send)
    "a" '("This buffer to context" . gptel-add)
    "f" '("File to context" . gptel-add-file)
    "r" '("Rewrite" . gptel-rewrite-menu)
    "c" '("Chat" . gptel)
    "p" '("Project chat" . madmas-gptel-project)
    "P" '("Set prompt" . gptel-system-prompt)
    "q" '("Abort" . gptel-abort))

  (defvar-keymap madmacs-copilot-chat-keys :doc "Keys for Copilot related functionality")
  (which-key-add-keymap-based-replacements madmacs-copilot-chat-keys
    "." '("Ask and insert" . copilot-ask-and-insert)
    "b" '("Add current buffer" . copilot-chat-add-current-buffer)
    "B" '("Del current buffer" . copilot-chat-del-current-buffer)
    "c" '("Chat display" . copilot-chat-display)
    "v" '("Commit Message" . copilot-chat-insert-commit-message)
    "e" '("Explain" . copilot-chat-explain)
    "r" '("Review" . copilot-chat-review)
    "f" '("Fix" . copilot-chat-fix)
    "o" '("Optimize" . copilot-chat-optimize)
    "d" '("Doc" . copilot-chat-doc)
    "t" '("Test" . copilot-chat-test)
    "T" '("Toggle" . copilot-toggle)
    "m" '("Toggle manual" . madmacs/copilot-manual-completion-toggle)
    "X" '("Log out" . copilot-logout)
    "l" '("Log in" . copilot-login)
    "p"  '("Prompt" . copilot-chat-custom-prompt-selection)
    "x" '("Reset" . copilot-chat-reset))
  
  (defvar-keymap madmacs-tools-keys :doc "Access various tools")
  (which-key-add-keymap-based-replacements madmacs-tools-keys
    "c" '("Calculator" . calc)
    "p" '("Pass" . pass))
  
  (defvar-keymap madmacs-leader-keys :doc "Everything you need fast under your finger tips")
  
  (which-key-add-keymap-based-replacements madmacs-leader-keys
    "!" `("Checkers" . ,madmacs-checker-keys)
    "." `("  Copilot" . ,madmacs-copilot-chat-keys)
    "," `("  Gptel" . ,madmacs-gptel-keys)
    "e" `("Filetree" . dirvish-side)
    "v" `("Git status". magit-status)
    "w" `("󰓩  Windows" . ,madmacs-windows-keys)
    "D"  `("  Docs" . ,madmacs-docs-keys)
    "d"  `("  Debugger" . ,madmacs-debugger-keys)
    "s" `("  Search" . ,search-map)
    "g" `("󰵉 Goto" . ,goto-map)
    "V" `("󰊢  Git" . ,madmacs-git-keys)
    "l" `("  LSP" . ,madmacs-lsp-keys)
    "L" `("  List" . ,madmacs-list-keys)
    "R" `("󰑮  Run" . ,madmacs-compiler-keys)
    "M" `("  Madmacs" . ,madmacs-madmacs-keys)
    "P" `("󰏖  Packages" . ,madmacs-packages-keys)
    "p" `("󱂬  Project" . ,madmacs-project-keys)
    "S" `("  Session" . ,madmacs-session-keys)
    "t" `(" Terminal" . ,madmacs-terminal-keys)
    "T" `(" Tools " . ,madmacs-tools-keys)
    "U" `("  UX" . ,madmacs-ux-keys))

  (keymap-set madmacs-mode-map "C-j" madmacs-leader-keys)
  (global-set-key (kbd "C-c") madmacs-leader-keys))


(provide 'madmacs-keys-keybindings)

