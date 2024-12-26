;; -*- lexical-binding: t; -*-

(use-package copilot
  :straight (copilot :type git :host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind
  (:map madmacs-mode-map
    ("C-c i" . copilot-complete))
  
  (:map madmacs-keymap-ai-copilot
    ("l" . copilot-login)
    ("R" . copilot-reset)
    ("m" . madmacs/copilot-manual-completion-toggle))

  (:map copilot-completion-map
    ("C-<tab>" . copilot-accept-completion)
    ("M-<tab>" . copilot-next-completion))
  
  :config
  (setopt copilot-idle-delay 0.3) ;; disable idle completion
  
  (defun madmacs/copilot-manual-completion-toggle ()
    "Toggle manual completion mode. When enabled, Copilot will NOT automatically complete the current symbol. Instead you will have to trigger manual via C-o"
    (interactive)
    (if copilot-idle-delay              ; automatic completion on
      (progn
        (setq copilot-idle-delay nil)
        (message "Copilot manual completion enabled"))
      (progn
        (setq copilot-idle-delay 0.3)
        (message "Copilot manual completion disabled"))))
  
  (setq warning-suppress-log-types '((copilot copilot-no-mode-indent)))
  
  :init
  (defvar-keymap madmacs-keymap-ai-copilot :doc "Copilot Keymap")
  (which-key-add-keymap-based-replacements madmacs-keymap-ai
    "c" `("Copilot" . ,madmacs-keymap-ai-copilot)))



(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (org copilot)
  :bind
  (:map vc-git-log-edit-mode-map
    ("C-c a" . copilot-chat-insert-commit-message))
  
  (:map madmacs-keymap-global
    ("i" . copilot-chat-ask-and-insert))
  
  (:map madmacs-keymap-ai-copilot
    ("." . copilot-chat-ask-and-insert)
    ("b" . copilot-chat-add-current-buffer)
    ("B" . copilot-chat-del-current-buffer)
    ("c" . copilot-chat-display)
    ("C" . copilot-chat-toggle)
    ("e" . copilot-chat-explain)
    ("r" . copilot-chat-review)
    ("f" . copilot-chat-fix)
    ("p" . copilot-chat-custom-prompt-selection)
    ("v" . copilot-chat-insert-commit-message))
 
   (:map copilot-chat-prompt-mode-map
    ;; avoid conflicts with gptel
     ("C-c C-c" . copilot-chat-prompt-send))

  :custom
  (copilot-chat-backend 'curl)
  (copilot-chat-frontend 'markdown)
  
  :init
  (with-eval-after-load 'popper
    (cl-pushnew 'copilot-chat-mode popper-reference-buffers))
  
  (which-key-add-keymap-based-replacements madmacs-keymap-ai
    "c" `("Copilot" . ,madmacs-keymap-ai-copilot)))

(provide 'madmacs-ai-copilot)
