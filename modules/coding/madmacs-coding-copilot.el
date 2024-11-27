;; -*- lexical-binding: t; -*-

(use-package copilot
  :ensure t
  :straight (copilot :type git :host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind
  (:map prog-mode-map
    ("C-o" . copilot-complete))

  (:map copilot-completion-map
    ("<tab>" . copilot-accept-completion)
    ("M-<tab>" . copilot-next-completion))
  
  :config

  (setq warning-suppress-log-types '((copilot copilot-no-mode-indent))))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (org)
  :bind
  (:map copilot-chat-prompt-mode-map
    ;; avoid conflicts with gptel
    ("C-c C-c" . copilot-chat-prompt-send))
  :custom
  (copilot-chat-backend 'curl)
  (copilot-chat-frontend 'markdown))


(provide 'madmacs-coding-copilot)
