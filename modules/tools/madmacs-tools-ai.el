;; -*- lexical-binding: t; -*-

(use-package gptel
  :straight (:host github :repo "karthink/gptel" :files ("*.el"))
  :init
  (setq gptel-expert-commands t)

  :config
	(defun make-key-function (secret-name)
    "Return a function that memoizes the key for SECRET-NAME."
    (let ((memoized-key nil))
      (lambda (&optional force-refresh)
        (when (or force-refresh (not memoized-key))
          (setq memoized-key
            (auth-source-pass-get 'secret secret-name)))
        memoized-key)))
  
  (gptel-make-anthropic "Claude"
    :stream t
    :key (make-key-function "anthropic.gptel"))
  
  (setq gptel-api-key (make-key-function "api.openai.com"))
  (setq gptel-default-mode 'org-mode)

  (setq gptel-model 'gpt-4o) ;; default model
  (with-eval-after-load 'embark
    (keymap-set embark-general-map "g" #'gptel-send)
    (keymap-set embark-region-map "g" #'gptel-send)
    (keymap-set embark-general-map "G" #'gptel-menu)
    (keymap-set embark-region-map "G" #'gptel-menu)))

(use-package gptel-quick
  :straight (:host github :repo "karthink/gptel-quick" :files ("gptel-quick.el"))
  :after gptel
  :config
  (with-eval-after-load 'embark
    (keymap-set embark-general-map "?" #'gptel-quick)))

(use-package elysium
  :straight (:host github :repo "lanceberge/elysium" :files ("*.el"))
  :custom
  (elysium-window-size 0.33)
  (elysium-window-style 'vertical))

(provide 'madmacs-tools-ai)
