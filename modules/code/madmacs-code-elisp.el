;; -*- lexical-binding: t; -*-

(use-package emacs-lisp-mode
  :straight nil
  :mode (("\\.el$" . emacs-lisp-mode))
  :interpreter (("emacs" . emacs-lisp-mode))
  :custom
  (lisp-indent-offset 2)

  :init
  (message "loading my elisp code")
  (defvar-keymap madmacs-elisp-local-leader-keys :doc "Local leader keymap for `emacs-lisp-mode' buffers.")

	(defvar-keymap madmacs-elisp-eval-keys :doc "Keymap for evaluating elisp expressions."
    "e" '("last sexp" . eval-last-sexp)
    "b" '("buffer" . eval-buffer)
    "r" '("region" . eval-region)
    "f" '("function" . eval-defun))

  (defvar-keymap madmacs-elisp-test-keys :doc "Keymap for testing elisp expressions."
    "b" '("buffer" . ert)
    "f" '("function" . ert-run-tests-interactively))

  (which-key-add-keymap-based-replacements madmacs-elisp-local-leader-keys
    "e" `("eval" . ,madmacs-elisp-eval-keys) 
    "t" `("test" . ,madmacs-elisp-test-keys)))

(use-package eldoc
  :straight nil
  :commands eldoc-mode
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
  :config
  ;; Show ElDoc messages in the echo area immediately, instead of after 1/2 a second.
  (setq eldoc-idle-delay 0))

;; better jump to definition
(use-package elisp-def
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

 

(provide 'madmacs-code-elisp)
