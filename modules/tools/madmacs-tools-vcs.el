;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t

  :bind
  ((:map goto-map
     ("v" . magit-status))
    
    (:map madmacs-keymap-vc
      ("." . magit-file-dispatch)
      ("," . magit-dispatch)
      ("s" . magit-status)
      ("b" . magit-blame)
      ("l" . magit-log)))
  
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
  :ensure t
  :bind
  (:map madmacs-keymap-vc
    ("t" . git-timemachine-toggle)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package smerge-mode
  :ensure nil
  :straight nil
  :bind
  (:map madmacs-keymap-vc
    ("m" . smerge-mode))
  
  ((:map smerge-mode-map
     ("C-c v m m" . smerge-keep-mine)
     ("C-c v m o" . smerge-keep-other)
     ("C-c v m b" . smerge-keep-base)
     ("C-c v m a" . smerge-keep-all)
     ("C-c v m c" . smerge-keep-current)
     ("C-c v m n" . smerge-next)
     ("C-c v m p" . smerge-prev))

    (:repeat-map smerge-repeat-map
      ("n" . smerge-next)
      ("p" . smerge-prev))))

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
