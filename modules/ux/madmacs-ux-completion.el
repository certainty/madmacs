;; -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :straight nil
  :custom
  (enable-recursive-minibuffers t) ; Use the minibuffer whilst in the minibuffer
  (completion-cycle-threshold 1)   ; TAB cycles candidates
  (completions-detailed t)         ; Show annotations
  (tab-always-indent 'complete)  ; When I hit TAB, try to complete, otherwise, i

  (completion-auto-help 'always) ; Open completion always; `lazy' another option
  (completions-max-height 30)    ; This is arbitrary
  (completions-format 'one-column)
  (completions-group t)
  (completion-auto-select 'second-tab)
  (suggest-keybindings t)

  :config
  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete))

(use-package minibuffer
  :ensure nil
  :straight nil
  :custom
  (completions-format 'one-column)
  (completion-show-help nil)
  (completion-auto-help 'always)
  (completion-auto-select nil)
  (completions-detailed t)
  (completion-show-inline-help nil)
  (completions-max-height 6)
  (completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
  (completions-highlight-face 'completions-highlight)
  (minibuffer-completion-auto-choose t)
  (completions-sort 'historical)
  (completion-styles '(orderless basic substring initials flex)))

(use-package orderless
  :ensure t
  :init
  (setopt
         completion-styles '(orderless basic partial-completion)
         completion-category-defaults nil
         completion-category-overrides '((file (styles . (partial-completion))))))

(use-package minibuf-eldef
  :ensure nil
  :straight nil
  :custom
  (minibuffer-default-prompt-format " [%s]")
  :config
  (minibuffer-electric-default-mode 1))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
          ("C-g" . #'vertico-exit)
          ("M-RET" . #'vertico-exit))
  :custom
  (vertico-cycle t)
  (vertico-resize nil)

  :init
  (vertico-mode))

(use-package vertico-repeat
  :ensure nil
  :straight nil
  :hook (minibuffer-setup . vertico-repeat-save)
  :commands (vertico-repeat-last))

(use-package vertico-directory
  :ensure nil
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy))

  :config
  (marginalia-mode))

(use-package helpful
  :ensure t)

(use-package consult
;; we replace a lot of functionality with the consult counter parts since they provide usually the better UX(use-package consult
  :ensure t
  :demand t
  :init
  (unbind-key "M-g n") ; free some prefix keys we need
  
  :bind
  (([remap Info-search] . consult-info)
    ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
    ("C-x b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
    ("C-x B" . consult-buffer)
    ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
    ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
    ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
    ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump

    ;; Custom M-# bindings for fast register access
    ("M-#" . consult-register-load)
    ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
    ("C-M-#" . consult-register)
    ;; Other custom bindings
    ("M-y" . consult-yank-pop)                ;; orig. yank-pop
    
    (:map goto-map
      ("i" . consult-imenu)

      ;; Notes relates
      ("n h" . consult-outline)
      ("n A" . consult-org-agenda)
      ("g" . consult-goto-line)
      ("M-g" . consult-goto-line)
      ("r" . consult-recent-file))
  
    (:map search-map
      ("g" . consult-ripgrep)
      ("l" . consult-line)
      ("v g" . consult-git-grep)
      (""))
    
    (:map isearch-mode-map
      ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
      ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
      ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
      ("M-s L" . consult-line-multi))            ;; needed by consult-line to detect isearch

    ;; Minibuffer history
    (:map minibuffer-local-map
      ("M-s" . consult-history)                 ;; orig. next-matching-history-element
      ("M-r" . consult-history)))
  
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (fset 'multi-occur #'consult-multi-occur)

  :custom
  (consult-narrow-key "<")
  (register-preview-delay 0.5)
  (register-preview-function 'consult-register-format)
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref)

  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file consult-theme
    :preview-key '(:debounce 0.2 any))


  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")
  (setq consult-async-min-input 2)

  ;; Consult info functions
  (defun consult-info-emacs ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "efaq" "elisp" "cl"))

  (defun consult-info-org ()
    "Search through the Org info page."
    (interactive)
    (consult-info "org"))

  (defun consult-info-completion ()
    "Search through completion info pages."
    (interactive)
    (consult-info "vertico" "consult" "marginalia" "orderless" "embark"
      "corfu" "cape" "tempel")))

(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-project-extra
  :ensure t
  :after consult
  :bind
  (:map goto-map
    ("*" . consult-project-extra-find)))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ;;;
;; ;;; In buffer completion
;; ;;;'(q)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-bar-width 0.5)
  (corfu-count 7)

  :bind
    (:map corfu-map
      ("C-SPC" . corfu-insert-separator)
      
      ("TAB" . nil)
      ("<return>" . nil)
      ("C-<TAB>" . corfu-insert)

      ("<escape>" . corfu-quit)
      ("C-g" . corfu-quit)
      ("C-j" . corfu-next)
      ("C-k" . corfu-previous))
  :init
  (global-corfu-mode 1))

;; Part of corfu

(use-package corfu-popupinfo
  :ensure nil
  :straight nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil))

(use-package cape
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Pretty icons for corfu
(use-package kind-icon
  :ensure t
  :custom
  (kind-icon-style 'nerd-icons)
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  :config
  (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter))


(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)

  :config
  (nerd-icons-completion-mode 1))

(use-package nerd-icons-corfu
  :ensure t
  :after (corfu marginalia)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; now finally let's bring in a good old friend [hippie-expand](https://www.emacswiki.org/emacs/HippieExpand)

(use-package emacs
  :ensure nil
  :straight nil
  :init
  (global-set-key [remap dabbrev-expand] 'hippie-expand))

(provide 'madmacs-ux-completion)
