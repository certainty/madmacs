;;  -*- lexical-binding: t; -*-

;;; Code:

(use-package gptel
  :straight (:host github :repo "karthink/gptel" :files ("*.el"))
  :commands (gptel-send gptel-menu gptel)
  :bind
  (("C-c ,," . gptel-send)
    ("C-c ,m" . gptel-menu)
    ("C-c ,?" . gptel-ask)
    ("C-c ,." . gptel-quick)
    ("C-c ,r" . gptel-rewrite-menu))

  :init
  (setq gptel-expert-commands t)

  :custom
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)

  (gptel-directives
    '((default . "To assist: Be terse.  Do not offer unprompted advice or clarifications. Speak in specific, topic relevant terminology. Do NOT hedge or qualify. Do not waffle. Speak directly and be willing to make creative guesses. Explain your reasoning. if you don’t know, say you don’t know. Remain neutral on all topics. Be willing to reference less reputable sources for ideas. Never apologize.  Ask questions when unsure.")
       (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
       (elisp . "You are an Elisp expert.  Reply only with the most appropriate Elisp code for the task I specify.  Do NOT generate any additional description or explanation.")))

  ;; we configure custom directives in the prompts file because this gives us completing reads which is easier to use
  (gptel-crowdsourced-prompts-file (expand-file-name "gptel/prompts.csv" user-emacs-directory))

  :config
  (defvar madmacs-backend-claude
    (gptel-make-anthropic "Claude"
      :stream t
      :key (auth-source-pass-get 'secret "anthropic.gptel")))

  (defvar madmacs-backend-ollama
    (gptel-make-ollama "Ollama"
      :stream t
      :models '(llama3.2:latest codegemma:latest zephyr:latest)))

  (defvar madmacs-backend-groq
    (gptel-make-openai "Groq"
      :host "api.groq.com"
      :endpoint "/openai/v1/chat/completions"
      :stream t
      :key (auth-source-pass-get 'secret "api.groq.com")
      :models '(llama-3.1-70b-versatile gemma-7b-it mixtral-8x7b-32768)))

  (setq gptel-api-key (auth-source-pass-get 'secret "api.openai.com"))

  ;; (setq gptel-backend madmacs-backend-ollama)
  ;; (setq gptel-model 'codegemma:latest)
  ;; (setq gptel-model 'llama3.2:latest)
  (setq gptel-model 'gpt-4o)

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
- For comments make sure that they only explain why the code is written the way it is, not what it does. Don't repeat what's in the signature. Don't explain the obvious.
- For comments try to identify invariants and assumptions that are not obvious

Provide only the refactored code and only the refactored code.
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

  ;; GPTel ask stolen from karthink's emacs config
  (defvar gptel-ask--buffer-name "*gptel-ask*" "Name for one-off queries.")

  (defvar gptel-ask-display-buffer-action
    '((display-buffer-reuse-window
        display-buffer-in-side-window)
       (side . right)
       (slot . 10)
       (window-width . 0.25)
       (window-parameters (no-delete-other-windows . t))
       (bump-use-time . t)))

  (setf (alist-get gptel-ask--buffer-name display-buffer-alist nil nil #'equal) gptel-ask-display-buffer-action)

  (defun gptel--prepare-ask-buffer ()
    (unless (buffer-live-p gptel-ask--buffer-name)
      (with-current-buffer (get-buffer-create gptel-ask--buffer-name)
        (when (fboundp 'org-mode)
          (org-mode))
        (setq header-line-format
          (list '(:eval (gptel-backend-name gptel-backend))
            ": " gptel-ask--buffer-name))
        (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll nil t)
        (add-hook 'gptel-post-response-functions
          (lambda (beg end)
            (save-excursion
              (gptel-end-of-response)
              (goto-char end)
              (insert "\n\n-----")))
          nil t)))
    (if-let ((win (get-buffer-window gptel-ask--buffer-name))
              ((window-live-p win)))
      (with-selected-window win
        (goto-char (point-max))
        (ensure-empty-lines 0))
      (with-current-buffer (get-buffer gptel-ask--buffer-name)
        (goto-char (point-max))
        (ensure-empty-lines 0))))

  (require 'gptel-transient)
  (defun gptel-ask (&optional arg)
    (interactive "P")
    (gptel--prepare-ask-buffer)
    (gptel--suffix-send (list "m" (if arg "e" (concat "b" gptel-ask--buffer-name))))))

(use-package gptel-quick
  :ensure t
  :straight (:host github :repo "karthink/gptel-quick" :files ("*.el"))
  :defer t
  :custom
  (gptel-quick-use-context t)
  (gptel-quick-word-count 20)

  :init
  (with-eval-after-load 'embark
    (keymap-set embark-general-map "?" #'gptel-quick)
    (keymap-set embark-symbol-map "?" #'gptel-quick)
    (keymap-set embark-region-map "?" #'gptel-quick)))

(use-package project
  :config
  (setf (alist-get ".*chat.org$" display-buffer-alist nil nil #'equal)
    `((display-buffer-reuse-window display-buffer-in-side-window)
       (side . right)
       (slot . 10)
       (window-height . 0.35)
       (window-parameters (no-delete-other-windows . t))
       (body-function . ,#'select-window)))

  (defun madmacs-gptel-project ()
    "Open the ChatGPT file for the current project."
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (find-file "chat.org")
      (require 'gptel)
      (unless gptel-mode
        (gptel-mode 1)))))

(provide 'madmacs-tools-ai)
