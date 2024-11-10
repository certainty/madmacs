;; -*- lexical-binding: t; -*-


(use-package lispy
  :ensure t
  :hook (lisp-mode emacs-lisp-mode scheme-mode racket-mode))

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
