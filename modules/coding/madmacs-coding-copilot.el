;; -*- lexical-binding: t; -*-

(use-package copilot
  :ensure t
  :straight (copilot :type git :host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind
  (:map prog-mode-map
    ("C-o" . copilot-complete))

  (:map copilot-completion-map
    ("C-y" . copilot-accept-completion)
    ("M-<tab>" . copilot-next-completion))
  
  :config
  (setopt copilot-idle-delay nil) ;; disable idle completion
  
  (defun madmacs/copilot-manual-completion-toggle ()
    "Toggle manual completion mode. When enabled, Copilot will NOT automatically complete the current symbol. Instead you will have to trigger manual via C-o"
    (interactive)
    (if copilot-idle-delay              ; automatic completion on
      (progn
        (setq copilot-idle-delay nil)
        (message "Copilot manual completion enabled"))
      (progn
        (setq copilot-idle-delay 0)
        (message "Copilot manual completion disabled"))))
  
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

