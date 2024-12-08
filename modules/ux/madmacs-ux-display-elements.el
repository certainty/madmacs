;; -*- lexical-binding: t; -*-

;;; Frames
(use-package frame
  :ensure nil
  :straight nil
  :hook (before-make-frame . window-divider-mode)
  :bind
  (:map madmacs-keymap-ux
    ("W" . toggle-frame-fullscreen))
  
  :custom
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only))

(use-package emacs
  :ensure nil
  :straight nil
  :custom
  (confirm-kill-emacs nil)
  (default-frame-alist '((frame-title-format . nil)
                         (internal-border-width . 2)
                         (tool-bar-lines . 0)
                         (vertical-scroll-bars . nil)
                         (horizontal-scroll-bars . nil))))


;;; Windows
(use-package window
  :straight nil
  :ensure nil
  :bind
  (:map global-map
    ("M-o" . other-window)
    ("C-x {" . shrink-window)
    ("C-x }" . enlarge-window)
    ("C-x >" . enlarge-window-horizontally)
    ("C-x <" . shrink-window-horizontally)
    (:repeat-map resize-window-repeat-map
      (">" . enlarge-window-horizontally)
      ("<" . shrink-window-horizontally)))
  :custom
  (display-buffer-base-action nil))

(use-package ace-window
  :ensure t
  :demand t
  :bind
  ((:map global-map
    ("C-x w w" . ace-window)
    ("C-x w x" . ace-swap-window)
    ("C-x w m" . ace-maximize-window))
  (:map goto-map
    ("w" . ace-window))))

(use-package windmove
  :straight nil
  :ensure nil
  :custom
  (windmove-default-keybindings nil))

(use-package winner
  :straight nil
  :ensure nil
  :config
  (winner-mode 1))

(use-package transpose-frame
  :ensure t
  :bind
  (:map global-map
    ("C-x w t" . transpose-frame)
    ("C-x w r" . rotate-frame-clockwise)
    ("C-x w f" . flip-frame)
    ("C-x w F" . flop-frame))
  
  (:map madmacs-keymap-windows
    ("f" . flip-frame)
    ("F" . flop-frame)
    ("r" . rotate-frame-clockwise)
    ("t" . tranpose-frame)))

(use-package popper
  :ensure t
  :bind (("C-`"  . popper-toggle)
          ("M-`"  . popper-cycle)
          ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
    '("\\*Messages\\*"
       "Output\\*$"
       "\\*Async Shell Command\\*"
       copilot-chat-mode
       sly-mrepl-mode
       help-mode
       compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;;; Buffers
(use-package emacs
  :ensure nil
  :straight nil
  :custom
  (auto-save-default nil)
  (auto-window-vscroll nil)
  (scroll-step 1)
  (scroll-margin 3)
  (scroll-conservatively 101)
  (scroll-up-aggressively 0.01)
  (scroll-down-aggressively 0.01)
  (fast-but-imprecise-scrolling nil)
  (hscroll-step 1)
  (hscroll-margin 1)
  (switch-to-buffer-preserve-window-point t)
  :config
  (add-to-list 'display-buffer-alist
    '((or . ((derived-mode . flymake-diagnostics-buffer-mode)
              (derived-mode . flymake-project-diagnostics-mode)
              (derived-mode . messages-buffer-mode)
              (derived-mode . backtrace-mode)))
       (display-buffer-reuse-mode-window display-buffer-at-bottom)
       (window-height . 0.3)
       (dedicated . t)
       (preserve-size . (t . t))))

  (add-to-list 'display-buffer-alist
    '("\\*\\(Output\\|Register Preview\\).*"
       (display-buffer-reuse-mode-window display-buffer-at-bottom)))

  (add-to-list 'display-buffer-alist
    '("\\`\\*Async Shell Command\\*\\'"
             (display-buffer-no-window))))

(use-package uniquify
  :ensure nil
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package whitespace
  :ensure nil
  :bind
  (("<f6>" . whitespace-mode)
   ("C-c z" . delete-trailing-whitespace)))


;; This doesn't play well will lsp / vcs as it slows down interaction considerably due to the timer functions it uses
(use-package autorevert
  :ensure nil
  :straight nil
  :config
  ;; if enabled causes lag with lsp and vcs 
  (global-auto-revert-mode nil)
  :custom
  (auto-revert-verbose nil)
  (auto-revert-interval .5)
  (auto-revert-check-vc-info nil)
  (rever-without-query '(".*")))

(use-package revert-buffer-all
  :ensure t
  :bind
  ("C-c b R" . revert-buffer-all))



(provide 'madmacs-ux-display-elements)
