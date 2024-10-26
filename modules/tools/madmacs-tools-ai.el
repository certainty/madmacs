;; -*- lexical-binding: t; -*-

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))
  :custom
  (chatgpt-shell-openai-key (lambda () (auth-source-pass-get 'secret "api.openai.com"))))

(use-package gptel
  :after embark
  :config
  (setq gptel-api-key (lambda () (auth-source-pass-get 'secret "api.openai.com")))
  (setq gptel-model 'gpt-4o-mini)
  (keymap-set embark-general-map "g" #'gptel-send)
  (keymap-set embark-region-map "g" #'gptel-send)
  (keymap-set embark-general-map "G" #'gptel-menu)
  (keymap-set embark-region-map "G" #'gptel-menu))

(use-package gptel-quick
  :straight (:host github :repo "karthink/gptel-quick" :files ("gptel-quick.el"))
  :after gptel
  :config
  (keymap-set embark-region-map "?" #'gptel-quick))

(provide 'madmacs-tools-ai)
