
;; -*- lexical-binding: t; -*-

(use-package paren
  :straight nil
  :hook (prog-mode . show-paren-mode)
  :custom
  (show-paren-delay 0))

(use-package highlight-parentheses
  :hook prog-mode)

(use-package electric-pair
  :straight nil
  :hook prog-mode)

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
          (text-mode . smartparens-mode)
          (markdown-mode . smartparens-mode))
  :bind
  (:map smartparens-mode-map
    ("M-(" . sp-wrap-round)
    ("C-)" . sp-forward-slurp-sexp)
    ("C-(" . sp-backward-slurp-sexp)
    ("C-}" . sp-forward-barf-sexp)
    ("C-{" . sp-backward-barf-sexp))
  
  :config
  (require 'smartparens-config))


(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
    `(("TODO" warning bold)
       ("FIXME" error bold)
       ("HACK" warning bold)
       ("DISCUSS" success bold)
       ("DEBUG" success bold)
       ("NOTE" success bold))))

(use-package emacs
  :straight nil
  :init
  (add-to-list 'display-buffer-alist
               '("\\*\\(Backtrace\\|Compile-log\\|Messages\\|Warnings\\)\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.33)
                 (window-parameters
                  (no-delete-other-windows . nil))))

  (add-to-list 'display-buffer-alist
               '("\\*shell:"
               (display-buffer-below-selected)
               (window-height . 12)))

  (add-to-list 'display-buffer-alist
               '("\\*\\(eldoc\\)\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (window-width . 30)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))
                 (dedicated . t)
                 (side . bottom)
                  (slot . 5))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Treesitter
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-font-lock-level 5) ;; make sure we get the elements highlighted we're interested in
  (treesit-auto-install 'prompt)
  (treesit-language-source-alist
    '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
       (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp"))
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (bash "https://github.com/tree-sitter/tree-sitter-bash/"))
  (treesit-auto-langs '(ruby scala rust typescript tsx javascript yaml dockerfile json html css lua elisp)) ; don't include go and gomod currently until I figure out how to change the tab-width
  :config
  (global-treesit-auto-mode))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Checkers
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flyspell
  :straight (:type built-in)
  :hook
  (prog-mode . flyspell-prog-mode)
  (conf-mode . flyspell-prog-mode)

  :bind
  (:map madmacs-keymap-global
    ("," . madmacs-flyspell-tmenu))

  :config
  (unbind-key "C-," flyspell-mode-map)
  (unbind-key "C-." flyspell-mode-map)
  (unbind-key "C-;" flyspell-mode-map)
  
  (require 'transient)
  
  (defun flyspell-correct-word ()
    "Run flyspell correction at point."
    (interactive)
    (if (fboundp 'flyspell-correct-at-point)
      (flyspell-correct-at-point)
      (flyspell-auto-correct-word)))

  (transient-define-prefix madmacs-flyspell-tmenu ()
    "Transient menu for Flyspell and Flymake."
    [["Flyspell Commands"
       ("c" "Correct Word" flyspell-correct-word :transient t)
       ("C" "Correct Word before" flyspell-correct-word-before-point :transient t)
       ("b" "Check Buffer" flyspell-buffer )
       ("r" "Check Region" flyspell-region)
       ("n" "Next Error" flyspell-goto-next-error :transient t)]
      ["Other"
        ("q" "Quit" transient-quit-one)]]))

(use-package flymake
  :straight (:type built-in)
  :custom
  (flymake-error-bitmap '(right-triangle compilation-error))
  (flymake-warning-bitmap '(right-triangle compilation-warning))
  (flymake-note-bitmap '(right-triangle compilation-info))
  (flymake-show-diagnostics-at-end-of-line nil) ;; enable this for inline hints
  
  :hook
  (prog-mode . flymake-mode)

  :bind
  (:map madmacs-keymap-global
    (";" . madmacs-flymake-tmenu))

  :config
  ;; a transient menu to manage flymake
  (require 'transient)
  (transient-define-prefix madmacs-flymake-tmenu ()
    "Transient menu for Flyspell and Flymake."
    [["Flymake Commands"
        ("d" "Show Diagnostics" consult-flymake)
        ("n" "Next Diagnostic" flymake-goto-next-error :transient t)
        ("p" "Previous Diagnostic" flymake-goto-prev-error :transient t)]
      ["Other"
        ("q" "Quit" transient-quit-one)]]))

(provide 'madmacs-code-essentials) 
