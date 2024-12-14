;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dabbrev
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :demand t
  :straight nil
  :init
  (global-set-key [remap dabbrev-expand] 'hippie-expand))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion styles
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; in buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :bind
  (:map corfu-map
	("C-SPC" . corfu-insert-separator))
  :custom
  (corfu-auto t)
  (tab-always-indent 'complete)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-bar-width 0.5)
  (corfu-count 10)
  (corfu-cycle t) ;; completions are a ring 

  (corfu-popupinfo-delay '(1.25 . 0.5))
  
  :config
  (corfu-popupinfo-mode 1)
  (global-corfu-mode 1)

  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(use-package kind-icon
  :after corfu
  
  :custom
  (kind-icon-style 'nerd-icons)
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  
  :init
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter)))


(use-package cape
  :demand t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; consult
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package consult
  :demand t
  :bind
  (:map madmacs-mode-map
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
	("M-y" . consult-yank-pop))                ;; orig. yank-pop

  (:map goto-map
	("i" . consult-imenu)
	("n h" . consult-outline)
	("n A" . consult-org-agenda)
	("M-g" . consult-goto-line)
	("r" . consult-recent-file))

  (:map search-map
	("g" . consult-ripgrep)
	("l" . consult-line)
	("v g" . consult-git-grep))

  (:map isearch-mode-map
	("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	("M-s L" . consult-line-multi))            ;; needed by consult-line to detect isearch

  ;; Minibuffer history
  (:map minibuffer-local-map
	("M-s" . consult-history)                 ;; orig. next-matching-history-element
	("M-r" . consult-history))

  :init
  (unbind-key "M-g n") ; free some prefix keys we need
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
      "corfu" "cape" "tempel"))

  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol))))

(use-package consult-dir
  :after vertico
  :bind
  (("C-x C-d" . consult-dir)
   :map vertico-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-project-extra
  :after consult
  :bind
  (:map goto-map
    ("g" . consult-project-extra-find)
    ("." . consult-project-extra-find)))

(use-package embark-consult
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'madmacs-completion)