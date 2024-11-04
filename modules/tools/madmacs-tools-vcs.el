;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :custom
  (git-commit-summary-max-length 80)
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  (magit-diff-refine-hunk t)
  (with-editor-emacsclient-executable "emacsclient")
  ;; this causes lag in buffers
  (magit-auto-revert-mode t)
  
  :config
  (setopt magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (add-to-list 'display-buffer-alist
             '("\\magit:"
                (display-buffer-same-window))))



(use-package git-timemachine
  :ensure t)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package smerge-mode
  :ensure nil
  :straight nil
  :bind
  (("C-c m m" . smerge-mode)
    (:map smerge-mode-map
      ("C-c m ." . smerge-keep-mine)
      ("C-c m o" . smerge-keep-other)
      ("C-c m b" . smerge-keep-base)
      ("C-c m a" . smerge-keep-all)
      ("C-c m c" . smerge-keep-current)
      ("C-c m n" . smerge-next)
      ("C-c m p" . smerge-prev)))

 	:init
  (which-key-add-key-based-replacements "C-c m" "smerge"))

;; add some embark actions to work on
(use-package embark-vc
  :ensure t
  :after embark)

(use-package ediff
  :ensure nil
  :straight nil
  :config
  (setopt ediff-diff-options "")
  (setopt ediff-custom-diff-options "-u")
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain)
  (setopt ediff-split-window-function 'split-window-vertically))

(provide 'madmacs-tools-vcs)
