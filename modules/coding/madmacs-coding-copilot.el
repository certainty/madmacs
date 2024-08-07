;; -*- lexical-binding: t; -*-

(use-package copilot
  :ensure t
  :straight (copilot :type git :host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("C-l" . copilot-accept-completion-by-line)
        ("C-y" . copilot-accept-completion)
        ("M-y" . copilot-accept-completion-by-word)
        ("C-p" . copilot-previous-completion)
        ("C-n" . copilot-next-completion))

  :config
  (setq warning-suppress-log-types '((copilot copilot-no-mode-indent))))

(provide 'madmacs-coding-copilot)
