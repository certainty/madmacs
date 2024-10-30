;; -*- lexical-binding: t; -*-

(use-package flyspell
  :ensure t
  :hook
  (prog-mode . flyspell-prog-mode)
  (conf-mode . flyspell-prog-mode))

(use-package flycheck
  :ensure t
  :custom
  (flycheck-highlighting-mode 'symbols)
  (flycheck-indication-mode nil)
  :config
  (global-flycheck-mode)
  (which-key-add-key-based-replacements "C-c !" "flycheck"))

(customize-set-variable 'flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))
(customize-set-variable 'flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters))

(provide 'madmacs-coding-checkers)
