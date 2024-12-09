;; -*- lexical-binding: t; -*-

(use-package vc
  :ensure nil
  :straight nil
  :bind
  (:map madmacs-mode-map
    ("C-x v B" . vc-annotate) 
    ("C-x v e" . vc-ediff)
    ("C-x v k" . vc-delete-file) 
    ("C-x v G" . vc-log-search) 
    ("C-x v t" . vc-create-tag)
    ("C-x v d" . vc-diff)
    ("C-x v ." . vc-dir-root) 
    ("C-x v <return>" . vc-dir-root)
    ("C-x v z p" . vc-git-stash-pop)
    ("C-x v z z" . vc-git-stash)
    ("C-x v z s" . vc-git-stash-snapshot)
   :map vc-dir-mode-map
    ("t" . vc-create-tag)
    ("O" . vc-log-outgoing)
    ("o" . vc-dir-find-file-other-window)
    ("d" . vc-diff) 
    ("k" . vc-dir-delete-file)
    ("G" . vc-revert)
   :map vc-git-stash-shared-map
    ("a" . vc-git-stash-apply-at-point)
    ("c" . vc-git-stash)
    ("k" . vc-git-stash-delete-at-point) ; symmetry with `vc-dir-delete-file'
    ("p" . vc-git-stash-pop-at-point)
    ("s" . vc-git-stash-snapshot)
   :map vc-annotate-mode-map
    ("M-q" . vc-annotate-toggle-annotation-visibility)
    ("C-c C-c" . vc-annotate-goto-line)
    ("<return>" . vc-annotate-find-revision-at-line)
   :map log-edit-mode-map
    ("M-s" . nil) ; I use M-s for my search commands
    
   :map log-view-mode-map
    ("<tab>" . log-view-toggle-entry-display)
    ("<return>" . log-view-find-revision)
    ("s" . vc-log-search)
    ("o" . vc-log-outgoing)
    ("f" . vc-log-incoming)
    ("F" . vc-update)
    ("P" . vc-push))
  :custom
  (vc-follow-symlinks t)
  
  :config
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  (setopt vc-handled-backends '(Git))
  (require 'log-edit)
  (setopt log-edit-confirm 'changed)
  (setopt log-edit-keep-buffer nil)
  (setopt log-edit-require-final-newline t)
  (setopt log-edit-setup-add-author nil)
  (remove-hook 'log-edit-hook #'log-edit-show-files)
  
  (setopt vc-find-revision-no-save t)
  (setopt vc-annotate-display-mode 'scale) ; scale to oldest
  (setopt add-log-keep-changes-together t)
  (setopt vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setopt vc-git-log-switches '("--stat"))
  (setopt vc-git-print-log-follow t)
  (setopt vc-git-revision-complete-only-branches nil) ; Emacs 28
  (setopt vc-git-root-log-format
        `("%d %h %ai %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
                   "\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) "
                   "\\(?4:[0-9]\\{4\\}-[0-9-]\\{4\\}[0-9\s+:-]\\{16\\}\\) "
                   "\\(?3:.*?\\):")
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  ;; These two are from Emacs 29
  (setopt vc-git-log-edit-summary-target-len 80)
  (setopt vc-git-log-edit-summary-max-len 120)

  (add-to-list 'display-buffer-alist
    '("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
       (display-buffer-reuse-mode-window display-buffer-below-selected)
       (window-height . 0.1)
       (dedicated . t)
       (preserve-size . (t . t)))))

(use-package agitate
  :ensure t
  :hook
  ((diff-mode . agitate-diff-enable-outline-minor-mode))
  :bind
  (:map madmacs-mode-map
    ("C-x v =" . agitate-diff-buffer-or-file) ; replace `vc-diff'
    ("C-x v g" . agitate-vc-git-grep) ; replace `vc-annotate'
    ("C-x v f" . agitate-vc-git-find-revision)
    ("C-x v s" . agitate-vc-git-show)
    ("C-x v w" . agitate-vc-git-kill-commit-message)
    ("C-x v p p" . agitate-vc-git-format-patch-single)
    ("C-x v p n" . agitate-vc-git-format-patch-n-from-head)
    :map diff-mode-map
    ("C-c C-b" . agitate-diff-refine-cycle) ; replace `diff-refine-hunk'
    ("C-c C-n" . agitate-diff-narrow-dwim)
    ("L" . vc-print-root-log)
    ("v" . vc-next-action)
    :map log-view-mode-map
    ("w" . agitate-log-view-kill-revision)
    ("W" . agitate-log-view-kill-revision-expanded)
    :map vc-git-log-view-mode-map
    ("c" . agitate-vc-git-format-patch-single)
    :map log-edit-mode-map
    ("C-c C-i C-n" . agitate-log-edit-insert-file-name)
    ("C-c C-i C-e" . agitate-log-edit-emoji-commit)
    ("C-c C-i C-c" . agitate-log-edit-conventional-commit))
  
  :config
  (advice-add #'vc-git-push :override #'agitate-vc-git-push-prompt-for-remote)

  (setq agitate-log-edit-informative-show-root-log nil
    agitate-log-edit-informative-show-files nil)
  
  (agitate-log-edit-informative-mode))

(use-package magit
  :ensure t

  :bind
  ((:map goto-map
     ("v" . magit-status)))
  
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


(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package smerge-mode
  :ensure nil
  :straight nil
  :bind
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

(use-package ediff
  :ensure nil
  :straight nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :custom
  (ediff-keep-variants nil)
  (ediff-make-buffers-readonly-at-startup nil)
  (ediff-merge-revisions-with-ancestor t)
  (ediff-show-clashes-only t))

(use-package diff-mode
  :ensure nil
  :straight nil
  :custom
  (diff-default-read-only t)
  (diff-advance-after-apply-hunk t)
  (diff-update-on-the-fly t)
  (diff-refine nil) 
  (diff-font-lock-prettify t) 
  (diff-font-lock-syntax 'hunk-also))

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
