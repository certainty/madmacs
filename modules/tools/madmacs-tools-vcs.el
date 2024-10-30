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
  :init
  (defun madmacs-smerge-try-smerge ()
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))
  (add-hook 'find-file-hook 'madmacs-smerge-try-smerge t)
  (add-hook 'after-revert-hook 'madmacs-smerge-try-smerge t))

;; add some embark actions to work on
(use-package embark-vc
  :ensure t
  :defer t
  :after embark)

(provide 'madmacs-tools-vcs)
