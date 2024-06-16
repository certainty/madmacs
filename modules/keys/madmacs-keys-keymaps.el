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

  (bind-keys :prefix-map madmacs-actions-map :prefix "C-c a" :prefix-docstring "Actions")
  (madmacs--describe-key "C-c a" "Actions")

  (bind-keys :prefix-map madmacs-buffer-map :prefix "C-c b" :prefix-docstring "Buffer")
  (madmacs--describe-key "C-c b" "Buffer")

  (bind-keys :prefix-map madmacs-goto-map :prefix "C-c j" :prefix-docstring "Goto")
  (madmacs--describe-key "C-c j" "Goto")

  (bind-keys :prefix-map madmacs-file-map :prefix "C-c f" :prefix-docstring "File")
  (madmacs--describe-key "C-c f" "File")

  (bind-keys :prefix-map madmacs-madmacs-map :prefix "C-c M" :prefix-docstring "Madmacs")
  (madmacs--describe-key "C-c M" "Madmacs")

  (bind-keys :prefix-map madmacs-open-map :prefix "C-c o" :prefix-docstring "Open")
  (madmacs--describe-key "C-c o" "Open")

  (bind-keys :prefix-map madmacs-project-map :prefix "C-c p" :prefix-docstring "Project")
  (madmacs--describe-key "C-c p" "Project")

  (bind-keys :prefix-map madmacs-search-map :prefix "C-c s" :prefix-docstring "Search")
  (madmacs--describe-key "C-c s" "Search")

  (bind-keys :prefix-map madmacs-toggle-map :prefix "C-c t" :prefix-docstring "Toggle")
  (madmacs--describe-key "C-c t" "Toggle")

  (bind-keys :prefix-map madmacs-vc-map :prefix "C-c v" :prefix-docstring "VCS")
  (madmacs--describe-key "C-c v" "VCS")

  (bind-keys :prefix-map madmacs-window-map :prefix "C-c w" :prefix-docstring "Window")
  (madmacs--describe-key "C-c w" "Window")

  (bind-keys :prefix-map madmacs-workspace-map :prefix "C-c TAB" :prefix-docstring "Workspaces")
  (madmacs--describe-key "C-c TAB" "Workspaces"))


(provide 'madmacs-keys-keymaps)
