;; -*- lexical-binding: t; -*-

(use-package bind-key
  :straight (:type built-in)
  :custom
  (bind-key-describe-special-forms nil))

(use-package repeat
  :straight (:type built-in)
  :hook after-init
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

  (:repeat-map madmacs-xref-repeat-map
    ("{" . xref-go-back)
    ("}" . xref-go-forward))

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

(use-package meow
  :demand t
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-use-cursor-position-hack t)
  (meow-use-clipboard t)
  (meow-goto-line-function 'consult-goto-line)
  (meow-keypad-leader-dispatch madmacs-keymap-global)
  (meow-char-thing-table
    '((?\( . round)
      (?\) . round)
      (?\[ . square)
      (?\] . square)
      (?{ . curly)
      (?} . curly)
      (?\" . string)
      (?s . symbol)
      (?w . window)
      (?b . buffer)
      (?p . paragraph)
      (?l . line)
      (?d . defun)
       (?. . sentence)))

  (meow-mode-state-list
    '((vterm-mode . insert)
       (vc-git-log-edit-mode . insert)
       (eshell-mode . insert)
       (vc-dir-mode . motion)
       (dired-mode . motion)
       (helpful-mode . motion)
       (help-mode . motion)
       (conf-mode . normal)
       (fundamental-mode . normal)
       (prog-mode . normal)
       (text-mode . normal)))

  :config
  (setq meow-use-dynamic-face-color nil)
  (setq meow--kbd-delete-char "<deletechar>")
  (with-eval-after-load 'org
    (modify-syntax-entry ?@ "_" org-mode-syntax-table))

  ;; These keybindings are intentionally close to emacs defaults
  (meow-normal-define-key
    '("0" . meow-expand-0)
    '("9" . meow-expand-9)
    '("8" . meow-expand-8)
    '("7" . meow-expand-7)
    '("6" . meow-expand-6)
    '("5" . meow-expand-5)
    '("4" . meow-expand-4)
    '("3" . meow-expand-3)
    '("2" . meow-expand-2)
    '("1" . meow-expand-1)

    ;; navigation
    '("p" . meow-prev) ; C-p
    '("n" . meow-next) ; C-n
    '("b" . meow-left) ; C-b
    '("f" . meow-right) ; C-f
    
    '("m" . back-to-indentation) ; M-m in emacs
    '("%" . meow-pop-to-mark) 
    '("M" . meow-unpop-to-mark)

    ;; scrolling
    '("j" . scroll-down-line)
    '("k" . scroll-up-line)
    
    ;; expansion
    '("P" . meow-prev-expand)
    '("N" . meow-next-expand)
    '("B" . meow-left-expand)
    '("F" . meow-right-expand)

    '("<" . meow-back-word) ; M-b
    '(">" . meow-next-word) ; M-f
    '("(" . meow-back-symbol) ; C-M-b
    '(")" . meow-next-symbol) ; C-M-f

    '("H" . meow-mark-word) 
    '("h" . meow-mark-symbol) 
    
    '("l" . meow-line) ; mnemonic line
        
    '("o" . meow-block) ; mnemonic blOck
    '("O" . meow-to-block) ; mnemonic blOck
    '("^" . meow-join) ; M-^

    `("g" . ,goto-map)
    '("G" . meow-grab) ; mnemonic grab
    '("-" . meow-pop-grab)
    '("x" . meow-swap-grab) ; mnemonic eXchange 
    '("X" . meow-sync-grab) ; 
    '("v" . meow-cancel-selection)
    '("V" . meow-pop-selection)

    '("t" . meow-till) 
    '("T" . meow-find)
    '("s" . meow-visit)
    '("S" . meow-search)
    '("r" . isearch-backward)

    '("[" . meow-beginning-of-thing)
    '("]" . meow-end-of-thing)
    '("{" . meow-inner-of-thing)
    '("}" . meow-bounds-of-thing)

    '("+" . expreg-expand)
    '("-" . expreg-contract)

    ;; editing
    '("w" . meow-kill) ; C-w
    '("W" . meow-save) ; M-w
    '("y" . meow-yank) ; C-y
    '("Y" . meow-yank-pop) ; M-y
    '("d" . meow-delete) ; C-d
    '("D" . meow-kill-whole-line) ; C-k
    '("c" . meow-change) 

    '("a" . meow-insert) ; C-a beginning of line
    '("i" . meow-insert) ; mnemonic Insert
    '("A" . meow-open-above) 
    '("e" . meow-append) ; C-e end of line
    '("E" . meow-open-below) 

    '("'" . meow-reverse) 
    '("_" . undo) ; C-_
    '("u" . undo) ; C-u
    '(";" . meow-comment) ; M-;
    '("z" . meow-repeat) ; C-z
    '("~" . negative-argument)

    ;; quick actions
    '("=" . indent-region)
    '("|" . eglot-code-actions) ; act on code
  
    '("\"" . embrace-commander)
    '("." . xref-find-definitions)
    '("?" . xref-find-references)
    '("q" . meow-quit)
    '("<escape>" . meow-cancel-selection))

  (add-to-list 'global-mode-string
    '("%e" (:eval (meow-indicator))))

  (meow-global-mode 1))

(use-package meow-tree-sitter
  :straight (meow-tree-sitter :type git :host github :repo "skissue/meow-tree-sitter")
  :after meow
  :init
  (meow-tree-sitter-register-defaults))

;; it is important to load which-key after meow to make sure the keymaps show up correctly
(use-package which-key
  :straight (:type built-in)
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


;; (use-package boon
;;   :demand t
;;   :hook
;;   (vc-dir-mode . turn-off-boon-mode)
;;   (vterm-mode . turn-off-boon-mode)
;;   :bind
;;   (:map boon-command-map
;;     ("_" . undo)
;;     ("." . xref-find-definitions)
;;     ("?" . xref-find-references)
;;     ("," . xref-pop-marker-stack)
;;     ("h" . avy-goto-char) ; mnemonic hop
;;     ("m" . embark-act) ; mnemonic menu
;;     ("v" . boon-copy-to-register)
;;     ("V" . insert-register)
;;     (":" . bookmark-set)
;;     ("#" . point-to-register))

;;   (:map boon-forward-search-map
;;     ("o" . occur)
;;     ("c" . nil)
;;     ("k" . nil))

;;   (:map boon-backward-search-map
;;     ("o" . occur)
;;     ("c" . nil)
;;     ("k" . nil))

;;   (:map boon-goto-map
;;     ("e" . nil)
;;     (":" . nil)
;;     ("w" . ace-window)
;;     ("f" . project-find-file)) ; restore sane bingings

;;   :config
;;   (require 'boon-emacs)
  
;;   ;; adivce the boon-reset-state-for-switchw function
;;   (defun madmacs-reset-state-unless-target-is-corfu-overlay (orig-fn frame)
;;     (unless (frame-parent frame)
;;       ;; no parent means this is not a child frame so we can reset
;;       (funcall orig-fn frame)))
  
;;   (advice-add 'boon-reset-state-for-switchw :around #'madmacs-reset-state-unless-target-is-corfu-overlay)
  
;;   (add-to-list 'global-mode-string
;;     '("%e" (:eval (boon-modeline-string))))

;;   (boon-mode 1))


(provide 'madmacs-base-keys)
