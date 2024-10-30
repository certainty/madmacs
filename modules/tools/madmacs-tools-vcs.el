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

(use-package forge
  :ensure t
  :after magit
  :config
  (push '("source.xing.com" "api.source.xing.com" "source.xing.com" forge-github-repository) forge-alist))

(use-package git-timemachine
  :ensure t)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package smerge-mode
  :ensure nil
  :straight nil
  :hook prog-mode)

(provide 'madmacs-tools-vcs)
