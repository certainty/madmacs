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
  (setopt use-short-answers t)
  
  (blink-cursor-mode 0)
  (global-hl-line-mode -1)
  (setq-default indent-tabs-mode nil)

  (defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

  (define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim))

(provide 'madmacs-ux-essentials)
