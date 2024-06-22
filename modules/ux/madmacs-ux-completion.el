
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
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("<escape>" . #'minibuffer-keyboard-quit)
              ("M-RET"    . #'vertico-exit))
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
  :ensure t
  :demand t
  :bind
  (("C-h i" . consult-info)
   (:map madmacs-project-map
         ("b" . consult-project-buffer)
         ("s" . consult-ripgrep)
         ("m" . consult-bookmark))
   (:map madmacs-buffer-map
         ("b" .  consult-buffer)
         ("m" .  consult-bookmark))
   (:map madmacs-goto-map
         ("l" . consult-line))
   (:map madmacs-file-map
         ("f" .  find-file)
         ("r" . consult-recent-file)
         ("o" .  consult-outline)
         ("a" .  consult-apropos)
         ("y" .  consult-yank-pop)
         ("g" .  consult-goto-line)))


  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (fset 'multi-occur #'consult-multi-occur)

  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file consult-theme
   :preview-key '(:debounce 0.2 any))

  ;; Configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setopt register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Use Consult to select xref locations with preview
  (setopt xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")
  (setq consult-async-min-input 2)

  ;; Consult info functions
  (defun consult-info-emacs ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "efaq" "elisp" "cl" "compat"))

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
         :map madmacs-project-map
         ("d" . consult-dir)
         :map madmacs-file-map
         ("d" . consult-dir)
         ("j" . consult-dir-jump-file)

         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-project-extra
  :ensure t
  :after consult
  :bind
  (:map madmacs-project-map
        ("F f" . consult-project-extra-find)
        ("F F" . consult-project-extra-find-other-window)))


;; ;;;
;; ;;; In buffer completion
;; ;;;

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)

  :bind
  (:map corfu-map
        ("C-SPC" . corfu-insert-separator)
        ("<escape>" . corfu-quit)
        ("<return>" . corfu-insert)
        ("TAB" . corfu-insert)
        ("C-g" . corfu-quit)
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous))
  :init
  (global-corfu-mode 1))

;; Part of corfu
(use-package corfu-popupinfo
  :ensure nil
  :straight nil
  :hook corfu
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
  (kind-icon-style 'alltheicons)
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

(provide 'madmacs-ux-completion)
