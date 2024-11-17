;; -*- lexical-binding: t; -*-


;; (use-package lispy
;;   :ensure t
;;   :hook (lisp-mode emacs-lisp-mode scheme-mode racket-mode))

(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode lisp-mode scheme-mode racket-mode))

;; (use-package smartparens
;;   :ensure smartparens  ;; install the package
;;   :hook ((prog-mode text-mode markdown-mode) . smartparens-strict-mode) ;; add `smartparens-mode` to these hooks
;;   :config
;;   ;; load default config
;;   (require 'smartparens-config))

;; Show matching parens
(use-package paren
  :ensure nil
  :straight nil
  :hook (prog-mode . show-paren-mode)
  :custom
  (show-paren-delay 0))

(use-package highlight-parentheses
  :ensure t
	:defer t
  :hook prog-mode)

(use-package electric-pair
  :ensure nil
  :straight nil
  :hook (emacs-lisp-mode lisp-mode))

(provide 'madmacs-coding-lisp)
