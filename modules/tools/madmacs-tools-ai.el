;; -*- lexical-binding: t; -*-

;;; Code:

(use-package gptel
  :straight (:host github :repo "karthink/gptel" :files ("*.el"))
  :init
  (setq gptel-expert-commands t)

  :custom
  (gptel-model 'gpt-4o)
  (gptel-default-mode 'org-mode)
  (gptel-directives
    '((default . "You are a large language model living in Emacs and a helpful assistant. You're an expert programmer and systems engineer. Respond concisely.")
       (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
       (common-lisp . "You are a large language model and a careful programmer specialized in modern common lisp. Provide code and only code as output without any additional text, prompt or note.")
       (writing . "You are a large language model and a writing assistant. Use simple and precise language. Respond concisely.")
       (chat . "You are a large language model and a conversation partner. Use simple and precise language. Respond concisely.")))

  :config
  (defun madmacs-auth-source-pass-get (secret)
    "Retrieves the secret from pass using the SECRET name and memoizes it so that repeated calls do not require a new pass lookup."
    (let ((memoized-secret nil))
      (lambda (&optional force-refresh)
        (when (or force-refresh (not memoized-secret))
          (setq memoized-secret
            (auth-source-pass-get 'secret secret)))
        memoized-secret)))

  (defun madmacs-open-ai-key-fn ()
    "Retrieves the OpenAI API key from pass."
    (madmacs-auth-source-pass-get "api.openai.com"))

  (defun madmacs-anthropic-key-fn ()
    "Retrieves the Anthropic API key from pass."
    (madmacs-auth-source-pass-get "anthropic.gptel"))

  (gptel-make-anthropic "Claude"
    :stream t
    :key (madmacs-anthropic-key-fn))

  (setq gptel-api-key (madmacs-open-ai-key-fn))

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

;; Ellama setup to use public models and also local ones

(use-package llm
  :straight (:host github :repo "ahyatt/llm" :files ("*.el"))
  :config
  (require 'llm-openai)
  (require 'llm-claude)
  (require 'llm-ollama)

  (defvar madmacs-llm-model-gpt4o nil)
  (defvar madmacs-llm-model-claude-sonnet nil)
  (defvar madmacs-llm-model-llama3.2 nil)
  (defvar madmacs-llm-model-zephyr nil)

  (defun madmacs-llm-setup-models ()
    (interactive)

    (unless madmacs-llm-model-llama3.2
      (setq madmacs-llm-model-llama3.2
        (make-llm-ollama
          :chat-model "llama3.2")))

    (unless madmacs-llm-model-zephyr
      (setq madmacs-llm-model-zephyr
        (make-llm-ollama
          :chat-model "zephyr")))
    
    (unless madmacs-llm-model-gpt4o
      (setq madmacs-llm-model-gpt4o
        (make-llm-openai
          :chat-model "gpt-4o"
          :key (auth-source-pass-get 'secret "api.openai.com"))))

    (unless madmacs-llm-model-claude-sonnet
      (setq madmacs-llm-model-claude-sonnet
        (make-llm-claude
          :chat-model "claude-3-5-sonnet-latest"
          :key (auth-source-pass-get 'secret "anthropic.gptel"))))))

(use-package ellama
  :after llm
  :defer t ;; defer is VERY important here, otherwise the interactive function to to get the password doesn't run properly and the API key is not set
  :commands (ellama-transient-main-menu)
  :bind
  (("C-c e" . ellama-transient-main-menu))
  :config
  (setopt llm-warn-on-nonfree nil)
  (setopt ellama-language "German")

  (madmacs-llm-setup-models)

  (setq ellama-providers
    `((llama3.2 . ,madmacs-llm-model-llama3.2)
       (zephyr . ,madmacs-llm-model-zephyr)
       (gpt4o . ,madmacs-llm-model-gpt4o)
       (claude-sonnet . ,madmacs-llm-model-claude-sonnet)))

  ;; (setq ellama-provider madmacs-llm-model-llama3.2)
  ;; (setq ellama-provider madmacs-llm-model-zephyr)
  (setq ellama-provider madmacs-llm-model-gpt4o)

  (with-eval-after-load 'embark
    (keymap-set embark-general-map "y" #'ellama-transient-main-menu)))

(provide 'madmacs-tools-ai)
