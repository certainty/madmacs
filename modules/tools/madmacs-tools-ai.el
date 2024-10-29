;; -*- lexical-binding: t; -*-

;;; Code:

(use-package gptel
  :straight (:host github :repo "karthink/gptel" :files ("*.el"))
  :bind
  (("C-c a" . gptel-menu)
    ("C-c C-<return>" . gptel-send))
  :init
  (setq gptel-expert-commands t)

  :custom
  (gptel-default-mode 'org-mode)

  ;; we configure custom directives in the prompts file because this gives us completing reads which is easier to use
  (gptel-crowdsourced-prompts-file (expand-file-name "gptel/prompts.csv" user-emacs-directory))
  
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

  (defvar madmacs-backend-claude
    (gptel-make-anthropic "Claude"
      :stream t
      :key (madmacs-anthropic-key-fn)))

	(defvar madmacs-backend-ollama
    (gptel-make-ollama "Ollama"
      :stream t
      :models '(llama3.2:latest codegemma:latest zephyr:latest)))

  (defvar madmacs-backend-groq
    (gptel-make-openai "Groq"
      :host "api.groq.com"
      :endpoint "/openai/v1/chat/completions"
      :stream t
      :key (madmacs-groq-key-fn)
      :models '(llama-3.1-70b-versatile gemma-7b-it mixtral-8x7b-32768)))

  (setq gptel-api-key (madmacs-open-ai-key-fn))
  (setq gptel-backend madmacs-backend-ollama)
  (setq gptel-model 'codegemma:latest)

	(when (file-exists-p gptel-crowdsourced-prompts-file)
    ;; touch the file because gptel checks if it's older than 14 days and attempts to refetch.
    ;; I do not want that.
    (f-touch gptel-crowdsourced-prompts-file))

  (defun madmacs-gptel-rewrite-directive ()
    "Sets a more specific rewrite directive for the gptel refactoring function"
    (if (derived-mode-p 'prog-mode)
      (format "You are a senior software engineer with extensive experience in %s. Your task is to refactor or rewrite the provided code while:
- Maintaining the exact same functionality and behavior
- Following %s best practices and idioms
- Improving code clarity and maintainability
- Adding clear, concise comments explaining complex logic
- Keeping variable/function names consistent with the original unless clarity can be improved
- Optimizing performance where possible without sacrificing readability
- Preserving any critical existing comments

Provide only the refactored code with necessary comments. Do not explain your changes or provide any other text.
The code is:
"
        (gptel--strip-mode-suffix major-mode)
        (gptel--strip-mode-suffix major-mode))

      (format "You are an expert editor and writing coach. Your task is to enhance the provided text while:
- Maintaining the original meaning and key points
- Improving clarity and readability
- Using active voice where appropriate
- Breaking down complex sentences
- Ensuring logical flow between paragraphs
- Eliminating redundancies and wordiness
- Using precise vocabulary
- Maintaining consistent tone
- Formatting the output in {MARKUP_LANGUAGE}

Return only the improved text formatted according to the specified markup language. Do not explain your changes or provide additional commentary.
The text is: ")))

  (setopt gptel-rewrite-directives-hook (list #'madmacs-gptel-rewrite-directive))
  
  (with-eval-after-load 'embark
    (keymap-set embark-general-map "." #'gptel-send)
    (keymap-set embark-region-map "." #'gptel-send)
    (keymap-set embark-general-map "," #'gptel-menu)
    (keymap-set embark-region-map "," #'gptel-menu)))

(provide 'madmacs-tools-ai)
