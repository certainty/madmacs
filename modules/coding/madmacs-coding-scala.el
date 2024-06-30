
(defvar-keymap madmacs-scala-local-leader-keys :doc "Local bindings for scala metals")

(use-package scala-mode
  :ensure t
  :interpreter ("scala" . scala-mode)
  :config
  (evil-define-key 'normal scala-mode-map (kbd "<localleader>") madmacs-scala-local-leader-keys))

(use-package lsp-metals
  :ensure t
  :hook (scala-mode . lsp)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.bsp\\'")

  (defvar-keymap madmacs-metals-server-keys :doc "Controlling the metals server")
  (which-key-add-keymap-based-replacements madmacs-metals-server-keys
    "d" '("Doctor" . lsp-metals-doctor-run)
    "l" '("Open log" . lsp-metals-open-server-log)
    "r" '("Reset workspace" . lsp-metals-reset-workspace))

  (defvar-keymap madmacs-metals-build-keys :doc "Controlling the metals build")
  (which-key-add-keymap-based-replacements madmacs-metals-build-keys
    "i" '("Import build" . lsp-metals-build-import)
    "c" '("Connect build" . lsp-metals-build-connect)
    "r" '("Restart build server" . lsp-metals-restart-build-server))

  (defvar-keymap madmacs-metals-compile-keys :doc "Controlling the metals compiler")
  (which-key-add-keymap-based-replacements madmacs-metals-compile-keys
    "c" '("Compile Clean" . lsp-metals-clean-compile)
    "C" '("Compile Cascade" . lsp-metals-cascade-compile)
    "q" '("Cancel compilation" . lsp-metals-cancel-compilation))

  (defvar-keymap madmacs-metals-test-keys :doc "Controlling the metals tests")
  (which-key-add-keymap-based-replacements madmacs-metals-test-keys
    "t" '("Test" . lsp-metals-treeview--buffer-changed))

  (which-key-add-keymap-based-replacements madmacs-scala-local-leader-keys
    "S" '("Analyze Stacktrace" . lsp-metals-analyze-stacktrace)
    "m" `("Metals" . ,madmacs-metals-server-keys)
    "b" `("Metals Build" . ,madmacs-metals-build-keys)
    "c" `("Metals Compile" . ,madmacs-metals-compile-keys)
    "t" `("Metals Test" . ,madmacs-metals-test-keys))

  )

(use-package sbt-mode
  :ensure t
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false"))

  (which-key-add-keymap-based-replacements madmacs-scala-local-leader-keys
    "s" '("SBT hydra" . sbt-hydra)))


(provide 'madmacs-coding-scala)
