;; -*- lexical-binding: t; -*-

(use-package sweeprolog
  :ensure t
  :straight (sweeprolog :type git :host github :repo "SWI-Prolog/packages-sweep" :files ("*.el" "*.pl" ".texi"))
  :hook
  (sweeprolog-mode . sweeprolog-electric-layout-mode)

  :custom
  (sweeprolog-swipl-path "/opt/homebrew/bin/swipl")

  :init
  (add-to-list 'auto-mode-alist '("\\.plt\\'" . sweeprolog-mode))
  (add-to-list 'auto-mode-alist '("\\.pl\\'" . sweeprolog-mode))
  (add-to-list 'auto-mode-alist '("\\.pro\\'" . sweeprolog-mode))
  (add-to-list 'auto-mode-alist '("\\.swipl\\'" . sweeprolog-mode))

  :config

  (defvar-keymap madmacs-sweeprolog-xref-keys :doc "Keymap for xref in sweeprolog")
  (which-key-add-keymap-based-replacements madmacs-sweeprolog-xref-keys
    "x" '("Xref" . sweeprolog-xref-project-source-files)
    "p" '("Predicate" .  sweeprolog-find-predicate)
    "m" '("Module" . sweeprolog-find-module)
    "t" '("Search term" . sweeprolog-term-search))

  (defvar-keymap madmacs-sweeprolog-refactoring-keys :doc "Keymap for refactoring in sweeprolog")

  (defvar-keymap madmacs-sweeprolog-help-keys :doc "Keymap for help in sweeprolog")
  (which-key-add-keymap-based-replacements madmacs-sweeprolog-help-keys
    "." '("Help thing"  . display-local-help)
    "d" '("Help predicate" .  eldoc-print-current-symbol-info)
    "p" '("Describe predicate" .  sweeprolog-describe-predicate)
    "D" '("Document predicate" .  sweeprolog-document-predicate-at-point)
    "m" '("Describe module"  . sweeprolog-describe-module)
    "i" '("Info manual"  . sweeprolog-info-manual))

  (defvar-keymap madmacs-sweeprolog-term-keys :doc "Keymap for terms in sweeprolog")
  (which-key-add-keymap-based-replacements madmacs-sweeprolog-term-keys
    "i" '("Insert term"  . sweeprolog-insert-term-dwim)
    "e" '("Export predicate"  . sweeprolog-export-predicate))

  (defvar-keymap madmacs-sweeprolog-listener-keys :doc "Keymap for the listener in sweeprolog")
  (which-key-add-keymap-based-replacements madmacs-sweeprolog-listener-keys
    "." '("Find file at point" .  sweeprolog-find-file-at-point)
    "s" '("Toplevel send goal" .  sweeprolog-top-level-send-goal)
    "b" '("Load buffer" .  sweeprolog-load-buffer))

  (defvar-keymap madmacs-sweeprolog-holes-keys :doc "Keymap for holes in sweeprolog")
  (which-key-add-keymap-based-replacements madmacs-sweeprolog-holes-keys
    "t" '("Insert term with holes" .  sweeprolog-insert-term-with-holes)
    "f" '("Forward hole" .  sweeprolog-forward-hole)
    "b" '("Backward hole" .  sweeprolog-backward-hole)
    "c" '("Count holes" .  sweeprolog-count-holes))

  (defvar-keymap madmacs-prolog-local-leader-keys :doc "Keymap for local leader keys for prolog")
  (which-key-add-keymap-based-replacements madmacs-prolog-local-leader-keys
    "h" `("Help" . ,madmacs-sweeprolog-help-keys)
    "x" `("Xref" . ,madmacs-sweeprolog-xref-keys)
    "r" `("Refactor" . ,madmacs-sweeprolog-refactoring-keys)
    "t" `("Term" . ,madmacs-sweeprolog-term-keys)
    "l" `("Listener" . ,madmacs-sweeprolog-listener-keys)
    "o" `("Holes" . ,madmacs-sweeprolog-holes-keys))

  (cl-case madmacs-modal-approach
    (evil
      (evil-define-key 'normal sweeprolog-mode-map (kbd ",") madmacs-prolog-local-leader-keys)
      (evil-define-key 'visual sweeprolog-mode-map (kbd ",") madmacs-prolog-local-leader-keys))
    (meow
      (define-key sweeprolog-mode-map (kbd "C-SPC") madmacs-prolog-local-leader-keys))
    (boon
      (define-key sweeprolog-mode-map (kbd "C-c ,") madmacs-prolog-local-leader-keys))))


(provide 'madmacs-coding-prolog)
