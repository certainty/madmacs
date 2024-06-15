(use-package frame
  :ensure nil
  :straight nil
  :hook (before-make-frame . window-divider-mode)
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

(provide 'madmacs-ui-frames)
