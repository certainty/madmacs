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

  (gptel-directives
    '((default . "You are a large language model living in Emacs and a helpful assistant. You're an expert programmer and systems engineer. Respond concisely.")

       (programming-expert .
         "You are a seasoned software engineer and architect. You act as an assistant living in my Emacs editor. Help me find solutions utilizing appropriate design patterns, good programming methods, and architectural principles.
Prioritize efficiency, scalability, security, simplicity and long-term maintainability. Respond concisely.")

       (writing . "You are a large language model and a writing assistant.
You are a skilled writer with a knack for creating captivating yet simple prose. You craft documents that engage the reader with expressive language, while maintaining clarity and simplicity. Respond concisely.")
       
       (chat . "You are a large language model and a conversation partner. Use simple and precise language. Respond concisely.")

       (common-lisp-expert . "You are a large language model and a careful programmer specialized in modern common lisp.
You have vast experience in common lisp and you use modern tools, that get the job done.
Favor simple code that leverages common lisp's strengths and does not try to be overly clever.
Respond concisely.")

       (code-review . "You are a large language model specialized in code reviews. Review the following code. You concentrate on:
1. Potential bugs and security issues
2. Performance optimizations
3. Code style and best practices
4. Architectural improvements
Please provide specific examples and recommendations.")
       
       (explain-code .
         "You're a large language model specialized in code comprehension and explanation. Explain this code to me. Focus on:
1. Main functionality
2. Key components
3. Dependencies
4. Important algorithms or patterns used
Respond concisely.")

       (tone-smoothing .
"Rewrite this text to be:
1. More professional and courteous
2. Clear and diplomatic
3. Maintaining the original message
4. Appropriate for business communication")

       (conciseness .
"Rewrite this text to be more concise while:
1. Preserving key information
2. Removing redundancy
3. Using clear, direct language
4. Maintaining professional tone")

       (git-commit-message .
"Generate a clear git commit message following conventional commits:
1. Type (feat/fix/docs/style/refactor/test/chore)
2. Scope (optional)
3. Brief description
4. Breaking changes if any
Format: type(scope): description")

       (generate-comments .
         "Generate comprehensive comments for this code:
1. Function/class purpose
2. Parameter descriptions
3. Return value details
4. Important implementation notes
5. Usage examples where appropriate")

       (generate-tests . "Generate test cases for this code:
1. Unit tests for core functionality
2. Edge cases and error conditions
3. Input validation tests
4. Integration test scenarios
Use appropriate testing framework conventions.
Provide code and only code for the tests.")))
  
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

  (with-eval-after-load 'embark
    (keymap-set embark-general-map "." #'gptel-send)
    (keymap-set embark-region-map "." #'gptel-send)
    (keymap-set embark-general-map "," #'gptel-menu)
    (keymap-set embark-region-map "," #'gptel-menu)))

(provide 'madmacs-tools-ai)
