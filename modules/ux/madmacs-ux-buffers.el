;; -*- lexical-binding: t; -*-

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
  (switch-to-buffer-preserve-window-point t))

(use-package uniquify
  :ensure nil
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; This doesn't play well will lsp / vcs as it slows down interaction considerably due to the timer functions it uses
;; (use-package autorevert
;;   :ensure nijl
;;   :straight nil
;;   :config
;;   (global-auto-revert-mode nil)
;;   :custom
;;   (auto-revert-verbose nil)
;;   (auto-revert-interval .5)
;;   (auto-revert-check-vc-info t)
;;   (rever-without-query '(".*"))
;;   )

(use-package revert-buffer-all
  :ensure t
  :bind
  ("C-c b R" . revert-buffer-all))

;; I am not convinced that I need this
;; (use-package popper
;;   :ensure t
;;   :bind (("M-`"   . popper-toggle-latest)
;;          ("C-`"   . popper-cycle)
;;          ("C-M-`" . popper-toggle-type))
;;   :custom
;;   (popper-window-height 20)
;;   (popper-display-control t)
;;   (popper-group-function #'popper-group-by-directory)

;;   (popper-reference-buffers
;;    '("\\*Messages\\*"
;;      "Output\\*$"
;;      "\\*Async Shell Command\\*"
;;      help-mode
;;      compilation-mode))

;;   :init
;;   (popper-echo-mode +1)

;;   :config
;;   (popper-mode))

(provide 'madmacs-ux-buffers)
