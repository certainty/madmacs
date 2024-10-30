;; -*- lexical-binding: t; -*-

;; (use-package evil-cleverparens
;;   :ensure t
;;   :if (eql madmacs-modal-approach 'evil)
;;   :hook (lisp-mode emacs-lisp-mode scheme-mode)
;;   :custom
;;   (evil-cleverparens-use-additional-bindings t))

(use-package lispy
  :ensure t
  :hook (lisp-mode emacs-lisp-mode scheme-mode))

(use-package lispyville
  :ensure t
  :if (eql madmacs-modal-approach 'evil)
  :hook (lisp-mode emacs-lisp-mode scheme-mode)
  :init
  (with-eval-after-load 'lispyville
    (lispyville-set-key-theme
      '(operator
         normal
         c-w
         c-u
         prettify
         text-objects
         (atom-motions (atom-movement t))
         additional-motions
         commentary
         slurp/barf-lispy
         wrap
         additional
         additional-insert
         additional-wrap
         (escape insert emacs)
         ;;  mark-special
         mark-toggle))
    :config
    ;; enter special mode after motion
    (setopt lispyville-motions-put-into-special t)
    (diminish 'lispyville-mode (lispyville-mode-line-string " λ•" " λ"))))

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
