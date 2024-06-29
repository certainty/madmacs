;; -*- lexical-binding: t; -*-

;; predefined maps which can be used to bind keys by the modules that we load later

(use-package emacs
  :ensure nil
  :straight nil
  :init

  (defun madmacs--describe-key (binding name)
    "Describe key binding with NAME if which-key is enabled"
    (when (featurep 'which-key)
      (which-key-add-key-based-replacements binding name)))

  (defun madmacs--describe-key-in-keymap (keymap binding name)
    "Describe key binding with NAME if which-key is enabled"
    (when (featurep 'which-key)
      (which-key-add-keymap-based-replacements keymap binding name)))
  ;; we try to avoid prefixes that would colide with meow's keypad keys (c,x,g,m)


  (defvar-keymap madmacs-buffers-keys
    :doc "Buffer related commands and utilities")

  (defvar-keymap madmacs-find-keys
    :doc "Keys which are related to finding and navigating things")

  (defvar-keymap madmacs-docs-keys
    :doc "Keys to find help and documentation")

  (defvar-keymap madmacs-lsp-keys
    :doc "LSP keys that are the same for all languages")

  (defvar-keymap madmacs-test-keys
    :doc "Test related keys for the different programming languages")

  (defvar-keymap madmacs-debugger-keys
    :doc "Keys related to debugging")

  (defvar-keymap madmacs-terminal-keys
    :doc "Keys related to interact with the terminal")

  (defvar-keymap madmacs-ux-keys
    :doc "Keys to control and toggle various aspects of the ux")

  (defvar-keymap madmacs-git-keys
    :doc "Keys to interact with git")

  (defvar-keymap madmacs-project-keys
    :doc "Keys related to interactions with the current project")

  (defvar-keymap madmacs-session-keys
    :doc "Keys to interact with the session / workspace")

  (defvar-keymap madmacs-compiler-keys
    :doc "Keys to build projects and interact with the compiler")

  (defvar-keymap madmacs-packages-keys
    :doc "Keys to interact with the package manager and the configuration")


  )


(provide 'madmacs-keys-keymaps)
