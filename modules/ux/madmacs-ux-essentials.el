;; -*- lexical-binding: t; -*-

(use-package so-long
  :ensure nil
  :straight nil
  :hook (after-init . global-so-long-mode))

(use-package simple
  :ensure nil
  :straight nil
  :hook (after-init . global-visual-line-mode)
  :custom
  (kill-whole-line t)
  (line-move-visual t))

(use-package display-line-numbers
  :ensure nil
  :straight nil
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width-start t))

(use-package casual-isearch
  :ensure t
  :bind (:map isearch-mode-map ("M-o" . casual-isearch-tmenu)))

(use-package casual-info
  :ensure t
  :bind (:map Info-mode-map ("M-o" . casual-info-tmenu)))

(use-package casual-bookmarks
  :ensure t
  :after bookmark
  :bind (:map bookmark-bmenu-mode-map ("M-o" . casual-bookmarks-tmenu)))

(use-package emacs
  :ensure nil
  :bind
  (:map global-map ("C-x f" . find-file))                   ; I don't want a short way to set the fill column. I want a short way to find a file
  
  :custom
  (sentence-end-double-space nil)
  (tab-width 2)
  (fill-column 150)
  (tab-always-indent 'complete)
  (completion-cycle-threshold 3)
  (require-final-newline nil)
  (visible-bell nil)
  (use-short-answers t)
  (undo-limit 67108864)
  (undo-strong-limit 100663296)
  :config
  (blink-cursor-mode 0)
  (global-hl-line-mode -1)
  (setq-default indent-tabs-mode nil))

(provide 'madmacs-ux-essentials)
