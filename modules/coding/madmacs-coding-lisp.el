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

(use-package emacs-lisp-mode
  :ensure nil
  :straight nil
  :mode (("\\.el$" . emacs-lisp-mode))
  :interpreter (("emacs" . emacs-lisp-mode))
  :custom
  (lisp-indent-offset 2))

(use-package eldoc
  :ensure nil
  :straight nil
  :commands eldoc-mode
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
  :diminish eldoc-mode
  :config
  ;; Show ElDoc messages in the echo area immediately, instead of after 1/2 a second.
  (setq eldoc-idle-delay 0))

;; better jump to definition
(use-package elisp-def
  :ensure t
  :commands (elisp-def elisp-def-mode)
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
    (add-hook hook #'elisp-def-mode))

  ;; Elisp hook
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
    (add-hook hook (lambda ()
                     (setq show-trailing-whitespace t)
                     (setq show-paren-context-when-offscreen t)
                     (prettify-symbols-mode 1)
                     (eldoc-mode 1)
                     (rainbow-delimiters-mode 1)))))

;; Show matching parens
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
  :hook (emacs-lisp-mode lisp-mode))

(provide 'madmacs-coding-lisp)
