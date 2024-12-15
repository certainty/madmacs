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
  :hook ((lisp-mode . smartparens-mode)
          (text-mode . smartparens-mode)
          (markdown-mode . smartparens-mode))
  :bind
  (:map smartparens-mode-map
    ("C-M-(" . sp-forward-slurp-sexp)
    ("C-M-)" . sp-backward-slurp-sexp)
    ("C-M-<" . sp-forward-barf-sexp)
    ("C-M->" . sp-backward-barf-sexp))
  
  :config
  (require 'smartparens-config))


(use-package svg-tag-mode
  :when (image-type-available-p 'svg)
  :hook (prog-mode . svg-tag-mode)
  :config
  (setq svg-tag-tags
    '(
       ("DONE:"  . ((lambda (tag) (svg-tag-make "DONE:"  :face 'fringe  :inverse t ))))
       ("FIXME:" . ((lambda (tag) (svg-tag-make "FIXME:" :face 'error   :inverse t))))
       ("HACK:"  . ((lambda (tag) (svg-tag-make "HACK:"  :face 'warning :inverse t))))
       ("NOTE:"  . ((lambda (tag) (svg-tag-make "NOTE:"  :face 'info :inverse t))))
       ("TODO:"  . ((lambda (tag) (svg-tag-make "TODO:"  :face 'warning :inverse t)))))))


(use-package fancy-compilation
    :hook compile-mode)

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
  :hook (after-startup . global-treesit-auto-mode)
  :custom
  (treesit-font-lock-level 4) ;; make sure we get the elements highlighted we're interested in
  (treesit-auto-install 'prompt)
  (treesit-language-source-alist
    '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
       (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp"))
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (bash "https://github.com/tree-sitter/tree-sitter-bash/"))
  (treesit-auto-langs '(ruby scala rust typescript javascript yaml dockerfile json html css lua elisp)) ; don't include go and gomod currently until I figure out how to change the tab-width
  :config
  (global-treesit-auto-mode))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Checkers
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flyspell
  :straight (:type built-in)
  :hook
  (prog-mode . flyspell-prog-mode)
  (conf-mode . flyspell-prog-mode))

(use-package flymake
  :straight (:type built-in)
  :hook
  (prog-mode . flymake-mode)
  :custom
  (flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))
  (flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters)))

(provide 'madmacs-code-essentials)
