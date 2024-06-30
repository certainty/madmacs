(use-package lispy
  :ensure t
  :hook (lisp-mode emacs-lisp-mode scheme-mode)
  :custom
  (lispy-safe-delete t)
  (lispy-safe-copy t)
  (lispy-safe-paste t)
  (lispy-safe-actions-no-pull-delimiters-into-comments t))

(use-package lispyville
  :ensure t
  :hook lispy-mode
  :config
  (lispyville-set-key-theme '(operators prettify text-objects c-w additional-motions commentary slurp barf wrap additional)))

(use-package emacs-lisp-mode
  :ensure nil
  :straight nil
  :mode (("\\.el$" . emacs-lisp-mode))
  :interpreter (("emacs" . emacs-lisp-mode)))

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
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0))

(provide 'madmacs-coding-lisp)
