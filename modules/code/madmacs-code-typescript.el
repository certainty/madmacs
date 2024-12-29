;; -*- lexical-binding: t; -*-
(use-package jest-test-mode)

(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-ts-mode)
  ("\\.tsx\\'" . tsx-ts-mode)
  :hook
  (typescript-mode . jest-test-mode)
  (typescript-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (typescript-ts-mode . jest-test-mode)
  (tsx-ts-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure))

(use-package polymode
  :init
  (define-hostmode poly-typescript-hostmode nil
    "Typescript hostmode."
    :mode 'typescript-ts-mode)
  (define-innermode poly-typescript-cssinjs-innermode nil
    :mode 'css-mode
    :head-matcher "\\(styled\\|css\\|\\.attrs<[^>]+>\\([^)]+\\)\\)?[.()<>[:alnum:]]?+`"
    :tail-matcher "\`"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-typescript-mode
    :hostmode 'poly-typescript-hostmode
    :innermodes '(poly-typescript-cssinjs-innermode))

  (add-to-list 'auto-mode-alist '("\\(styled\\|style[sd]\\).[tj]sx?\\'" . poly-typescript-mode)))

(use-package prettier-js
  :hook (typescript-mode typescrip-ts-mode))

(use-package flymake-eslint
  :config
  (setq flymake-eslint-prefer-json-diagnostics t)

  (defun madmacs-use-local-eslint ()
    "Set project's `node_modules' binary eslint as first priority.
If nothing is found, keep the default value flymake-eslint set or
your override of `flymake-eslint-executable-name.'"
    (interactive)
    (let* ((root (locate-dominating-file (buffer-file-name) "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/.bin/eslint"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flymake-eslint-executable-name eslint)
        (message (format "Found local ESLINT! Setting: %s" eslint))
        (flymake-eslint-enable))))

  (defun madmacs-configure-eslint-with-flymake ()
    (when (or (eq major-mode 'tsx-ts-mode)
              (eq major-mode 'typescript-ts-mode)
              (eq major-mode 'typescriptreact-mode))
      (madmacs-use-local-eslint)))

  (add-hook 'eglot-managed-mode-hook #'madmacs-use-local-eslint))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(provide 'madmacs-code-typescript)
