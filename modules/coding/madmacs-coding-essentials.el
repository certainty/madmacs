;; -*- lexical-binding: t; -*-

(use-package paren
  :ensure nil
  :straight nil
  :hook (prog-mode . show-paren-mode)
  :custom
  (show-paren-delay 0))

(use-package highlight-parentheses
  :ensure t
  :hook prog-mode)

(use-package electric-pair
  :ensure nil
  :straight nil
  :hook prog-mode)

(use-package smartparens
  :ensure t
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
  :ensure t
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

(use-package quickrun
  :ensure t)

(use-package fancy-compilation
    :ensure t
    :commands (fancy-compilation-mode)
    :init
    (with-eval-after-load 'compile
      (fancy-compilation-mode)))

(use-package emacs
  :ensure nil
  :straight nil
  :init
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'electric-pair-mode)

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

(provide 'madmacs-coding-essentials)
