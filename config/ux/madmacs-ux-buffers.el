(use-package emacs
  :ensure nil
  :straight nil
  :custom
  (auto-window-vscroll nil)
  (scroll-step 1)
  (scroll-margin 3)
  (scroll-conservatively 101)
  (scroll-up-aggressively 0.01)
  (scroll-down-aggressively 0.01)
  (fast-but-imprecise-scrolling nil)
  (hscroll-step 1)
  (hscroll-margin 1)
  (switch-to-buffer-preserve-window-point t))

(use-package uniquify
  :ensure nil
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package autorevert
  :ensure nil
  :straight nil
  :config
  (global-auto-revert-mode 1)
  :custom
  (auto-revert-verbose nil)
  (auto-revert-interval .5)
  (auto-revert-check-vc-info t)
  (rever-without-query '(".*")))

(use-package revert-buffer-all
  :ensure t
  :bind
  ("C-c b R" . revert-buffer-all))

(use-package popper
  :ensure t
  :bind (("M-`"   . popper-toggle-latest)
         ("C-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-window-height 20)
  (popper-display-control t)
  (popper-group-function #'popper-group-by-directory)

  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     help-mode
     compilation-mode))

  :init
  (popper-echo-mode +1)

  :config
  (popper-mode))

(use-package mwheel
  :ensure nil
  :straight nil
  :custom
  (mouse-wheel-follow-mouse t)
  (mouse-wheel-progressive-speed t)
  (mouse-wheel-coalesce-scroll-events t)
  (mouse-wheel-scroll-amount '(1 ((shift) . 2))))

(use-package pixel-scroll
  :ensure nil
  :disabled)

(provide 'madmacs-ux-buffers)
