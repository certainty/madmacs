;; -*- lexical-binding: t; -*-

(use-package bind-key
  :straight nil
  :custom
  (bind-key-describe-special-forms nil))

(use-package which-key
  :straight nil
  :demand t
  :custom
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-max-display-columns 10)
  (which-key-show-early-on-C-h t)
  (which-key-show-prefix nil)
  (which-key-idle-delay .75)
  (which-key-idle-secondary-delay 0.05)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-setup-minibuffer)
  (which-key-separator " • ")
  (which-key-prefix-prefix nil)

  :init
  (which-key-mode))

(use-package repeat
  :straight nil
  :demand t
  :custom
  (repeat-exit-timeout 3)

  :config
  (dolist (command '(next-error previous-error))
    (put command 'repeat-exit-timeout 'no))
  
  :bind
  (:map goto-map
    ("]" . next-buffer)
    ("[" . previous-buffer))

  (:repeat-map madmacs-buffer-repeat-map
    ("]" . next-buffer)
    ("[" . previous-buffer))
  
  ;; Object-specific Motion Repeat 
  (:repeat-map madmacs-line-repeat-map
    ("^" . join-line))

  (:repeat-map madmacs-paragraph-repeat-map
    ("}" . forward-paragraph)
    ("{" . backward-paragraph))
  
  (:repeat-map madmacs-undo-repeat-map
    ("u" . undo))

  (:repeat-map madmacs-scroll-repeat-map
    ("v" . scroll-up-command)
    ("M-v" . scroll-down-command)
    ("l" . recenter-top-bottom))

  (:repeat-map madmacs-windows-repeat-map
    ("o" . other-window))
  
  :init
  (defun madmacs--repeatize (keymap)
    "Add `repeat-mode' support to an existing KEYMAP.
    If you define the keymap new via defvar-keymap use the :repeat property instead"
    (map-keymap
      (lambda (_key cmd)
        (when (symbolp cmd)
          (put cmd 'repeat-map keymap)))
      (symbol-value keymap))))

(use-package emacs
  :straight nil
  :demand t
  :bind
  (:map madmacs-mode-map
	("C-z" . repeat) ; I don't use suspend frame so this is a nicer binding than C-x z
	("C-x z" . repeat-complex-command)) ; now this can go here
  
  (:map madmacs-keymap-packages
    ("u" . straight-pull-package)
    ("U" . straight-pull-all))
  (:map goto-map
    ("#" . jump-to-register))

  :config
  (unbind-key "M-g n")
  (unbind-key "C-j")
  
  (defvar-keymap madmacs-keymap-global :doc "The keymap used for global commands. This is the home of keys that don't live in other global maps like C-x or C-c")
  (defvar-keymap madmacs-keymap-ux :doc "UX settings")
  (defvar-keymap madmacs-keymap-packages :doc "Package functionality")
  (defvar-keymap madmacs-keymap-ai :doc "AI related functionality")
  
  (which-key-add-keymap-based-replacements madmacs-keymap-global
    "p" `("Packages" . ,madmacs-keymap-packages)
    "u" `("UX" . ,madmacs-keymap-ux)
    "l" `("AI" . ,madmacs-keymap-ai))

  (which-key-add-keymap-based-replacements global-map
    "C-j" `("Madmacs" . ,madmacs-keymap-global))
  
  (which-key-add-keymap-based-replacements madmacs-mode-map
    "C-j" `("Madmacs" . ,madmacs-keymap-global)))

(use-package boon
  :demand t
  :hook
  (vc-dir-mode . turn-off-boon-mode)
  (vterm-mode . turn-off-boon-mode)
  :bind
  (:map boon-command-map
    ("_" . undo)
    ("." . xref-find-definitions)
    ("?" . xref-find-references)
    ("," . xref-pop-marker-stack)
    ("h" . avy-goto-char) ; mnemonic hop
    ("m" . embark-act) ; mnemonic menu
    ("v" . boon-copy-to-register)
    ("V" . insert-register)
    (":" . bookmark-set)
    ("#" . point-to-register))
  
  (:map boon-forward-search-map
    ("o" . occur)
    ("c" . nil)
    ("k" . nil))
  
  (:map boon-backward-search-map
    ("o" . occur)
    ("c" . nil)
    ("k" . nil))
  
  (:map boon-goto-map
    ("e" . nil)
    (":" . nil)
    ("w" . ace-window)
    ("f" . project-find-file)) ; restore sane bingings
  
  :config
  (require 'boon-emacs)
  ;(remove-hook 'window-selection-change-functions #'boon-reset-state-for-switchw)
  
  (add-to-list 'global-mode-string
    '("%e" (:eval (boon-modeline-string))))

  (boon-mode 1))

(provide 'madmacs-base-keys)
