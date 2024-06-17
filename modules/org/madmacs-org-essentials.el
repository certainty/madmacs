
;; this is taken almost verbatim from lambda-emacs which is great when it comes to note-taking

(use-package org
  :ensure nil
  :straight nil
  :commands (org-mode)
  :mode (("\\.org$" . org-mode))
  ;:hook (org-mode . madmacs--nicer-org)
  :bind
  (:map madmacs-notes-keymap
        ("a" . org-agenda))
  (:map org-mode-map
        ("C-M-k" . org-metaup)
        ("C-M-j" . org-metadown)
        ("C-M-l" . org-metaright)
        ("C-M-h" . org-metaleft)
        ("M-J" . org-shiftdown)
        ("M-K" . org-shiftup)
        ("M-L" . org-shiftright)
        ("M-H" . org-shiftleft))

  :init
  ;; Org-Emphasis-Regex settings. Set regex boundaries for emphasis.
  ;; Load this before org-mode is loaded.
  ;; See https://emacs.stackexchange.com/q/54673/11934
  ;; https://emacs.stackexchange.com/q/54632/11934

  (setq org-emphasis-regexp-components
        '("-—[:space:]('\"{["
          "\] - [:space:].,:!?;'\")}\\["
          "[:space:]"
          "."
          1))

  :custom
  ;; Aesthetics & UI
  (org-auto-align-tags nil)              ;; don't auto-align tags
  (org-catch-invisible-edits 'smart)     ;; prevent editing invisible area
  (org-cycle-separator-lines 0)          ;; no empty lines in collapsed view
  (org-ellipsis "…")                     ;; nicer elipses "↷" "↴" "▼"
  (org-fontify-quote-and-verse-blocks t) ;; make quotes stand out
  (org-hide-emphasis-markers t)          ;; hide emph markers
  (org-hide-leading-stars t)             ;; hide leading stars
  (org-image-actual-width  500) ;; show all images at 500px using imagemagik
  (org-pretty-entities t)       ;; make latex look good, etc.
  (org-pretty-entities-include-sub-superscripts t) ;; prettify sub/superscripts
  (org-read-date-prefer-future 'time) ;; Incomplete dates refer to future dates & times
  (org-startup-folded nil)            ;; Don't start org in outline
  (org-tags-column 0) ;; place tags directly next to headline text

  ;; Footnotes
  (org-footnote-section nil)   ;; place footnotes locally
  (org-footnote-auto-adjust t) ;; renumber footnotes

  ;; Indentation
  (org-adapt-indentation t)        ;; adapt indentation
  (org-startup-indented t)         ;; start with indentation of headlines
  (org-src-preserve-indentation t) ;; preserve code indentation

  ;; Insertion/Yanking
  (org-insert-heading-respect-content t) ;; insert new headings after subtree
  (org-M-RET-may-split-line '((default . t))) ;; don't split line when creating a new headline, list item, or table field
  (org-yank-adjusted-subtrees t) ;; adjust subtrees to depth when yanked
  (org-yank-folded-subtrees t)   ;; fold subtrees on yank

  ;; Lists
  (org-list-allow-alphabetical t) ;; allow alphabetical list
  ;; Demote sequence for list bullets
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  (org-list-indent-offset 1) ;; increase sub-item indentation

  ;; Logging
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-log-redeadline nil) ;; don't log the time a task was rescheduled/redeadlined.
  (org-log-reschedule nil)

  ;; Movement
  (org-return-follows-link t) ;; make RET follow links
  (org-special-ctrl-a/e t)    ;; better movement in headers

  ;; Searching
  (org-imenu-depth 8)   ;; scan to depth 8 w/imenu
  (imenu-auto-rescan t) ;; make sure imenu refreshes

  ;; Source block settings
  (org-src-fontify-natively t)         ;; use lang-specific fontification
  (org-src-window-setup 'other-window) ;; edit source in other window
  (org-src-tab-acts-natively t)        ;; use lang bindings
  (org-confirm-babel-evaluate t)       ;; confirm evaluation

  ;; TODOS
  (org-use-fast-todo-selection 'expert) ;; don't use popup window for todos
  ;; don't set to DONE if children aren’t DONE
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)

  :config
  (defun madmacs--nicer-org ()
    (interactive)

    (org-modern-mode 1)

    (hl-line-mode -1)
    (display-line-numbers-mode -1)

    (flycheck-mode -1)
    (org-indent-mode t)

    (setq visual-fill-column-width 250)
    (setq visual-fill-column-center-text t)

    (visual-line-mode 1))

  (setopt org-directory madmacs-org-directory)
  (setopt org-attach-id-dir (concat madmacs-org-directory "/.attachments"))

  (setopt org-hide-emphasis-markers t)
  (setopt org-display-inline-images t)

  (require 'org-faces)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font dk/variable-width-font :weight 'medium :height (cdr face)))

  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)


  (setq org-refile-targets '((nil :maxlevel . 5)
                             (org-agenda-files :maxlevel . 5)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t))

;; make the real code appear under certain org structures. This reveals the underlying markup if you need it
(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setopt org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t
        org-appear-delay 0.5
        org-appear-trigger-commands '(org-cycle)))

;; use org-modern
(use-package org-modern
  :ensure t
  :after org
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda))

  :custom
  (org-modern-label-border 0.1)
  (org-modern-block-fringe 1))

(provide 'madmacs-org-essentials)
