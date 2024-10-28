;; -*- lexical-binding: t; -*-

;;; Code:

;; I try to use gptel for most of my AI needs, but I also have llm and ellama setup for more specialized tasks.
;; GPTel sets out to be unobstrusive and get out of the way, which I very much appreciate.

(use-package gptel
  :straight (:host github :repo "karthink/gptel" :files ("*.el"))
  :bind
  (("C-c a" . gptel-menu)
   ("C-c C-<return>" . gptel-send))
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

  (defun madmacs-groq-key-fn ()
    "Retrieves the Groq API key from pass."
    (madmacs-auth-source-pass-get "api.groq.com"))

  (gptel-make-anthropic "Claude"
    :stream t
    :key (madmacs-anthropic-key-fn))

  (gptel-make-ollama "Ollama"
    :stream t
    :models '(llama3.2:latest codegemma:latest zephyr:latest))

  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key (madmacs-groq-key-fn)
    :models '(llama-3.1-70b-versatile gemma-7b-it mixtral-8x7b-32768))
  
  (setq gptel-api-key (madmacs-open-ai-key-fn))

  (with-eval-after-load 'embark
    (keymap-set embark-general-map "g" #'gptel-send)
    (keymap-set embark-region-map "g" #'gptel-send)
    (keymap-set embark-general-map "G" #'gptel-menu)
    (keymap-set embark-region-map "G" #'gptel-menu)))

(provide 'madmacs-tools-ai)
