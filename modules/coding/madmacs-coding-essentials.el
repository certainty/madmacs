(use-package svg-tag-mode
  :ensure t
  :when (image-type-available-p 'svg)
  :hook (prog-mode . svg-tag-mode)
  :config
  (setq svg-tag-tags
        '(;; Replaces any occurence of :XXX: with a dynamic SVG tag displaying XXX
          ("\\(:[A-Z]+:\\)" . ((lambda (tag)
                                 (svg-tag-make tag :face 'success :inverse t :beg 1 :end -1))))
          ;; other tags
          ("DONE:"  . ((lambda (tag) (svg-tag-make "DONE:"  :face 'fringe  :inverse t ))))
          ("FIXME:" . ((lambda (tag) (svg-tag-make "FIXME:" :face 'error   :inverse t))))
          ("HACK:"  . ((lambda (tag) (svg-tag-make "HACK:"  :face 'warning :inverse t))))
          ("NOTE:"  . ((lambda (tag) (svg-tag-make "NOTE:"  :face 'info :inverse t))))
          ("TODO:"  . ((lambda (tag) (svg-tag-make "TODO:"  :face 'warning :inverse t)))))))

(use-package quickrun
  :ensure t)

(use-package emacs
  :ensure nil
  :straight nil
  :hook
  (prog-mode . electric-pair-mode)
  (prog-mode . display-line-number-mode)

  :init
  (add-to-list 'display-buffer-alist
               '("\\*\\(Backtrace\\|Compile-log\\|Messages\\|Warnings\\)\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.33)
                 (window-parameters
                  (no-delete-other-windows . nil))))

  (add-to-list 'display-buffer-alist
               '("\\*shell:"
               (display-buffer-below-selected)
               (window-height . 12)))

  (add-to-list 'display-buffer-alist
               '("\\*\\(eldoc\\)\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (window-width . 30)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))
                 (dedicated . t)
                 (side . bottm)
                 (slot . 5))))

(use-package treesit
  :ensure nil
  :straight nil
  :custom
  (treesit-extra-load-path (list (concat madmacs--data-dir "ts/")))
  (major-mode-remap-alist '((c++-mode . c++-ts-mode)
                            (c-mode . c-ts-mode)
                            (c-or-c++-mode . c-or-c++-ts-mode)
                            (conf-toml-mode . toml-ts-mode)
                            (csharp-mode . csharp-ts-mode)
                            (css-mode . css-ts-mode)
                            (java-mode . java-ts-mode)
                            (js-json-mode . json-ts-mode)
                            (python-mode . python-ts-mode)
                            (ruby-mode . ruby-ts-mode)
                            (sh-mode . bash-ts-mode)))
  :init
  (setq treesit-language-source-alist
        '(
          (css   . ("https://github.com/tree-sitter/tree-sitter-css"))
          (go    . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod .    ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (scala      . ("https://github.com/tree-sitter/tree-sitter-scala"))
          (graphql . ("https://github.com/bkegley/tree-sitter-graphql"))
          (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (elixir    . ("https://github.com/elixir-lang/tree-sitter-elixir"))
          (elm        . ("https://github.com/razzeee/tree-sitter-elm"))
          (erlang     . ("https://github.com/WhatsApp/tree-sitter-erlang"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))))

  (defun madmacs--treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
        (treesit-install-language-grammar lang)
        (message "`%s' parser was installed." lang)
        (sit-for 0.75))))

  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (json-mode . json-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (go-mode . go-ts-mode)
          (go-mod-mode . go-mod-ts-mode)
          (graphql-mode . graphql-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (rust-mode . rust-ts-mode)
          (scala-mode . scala-ts-mode)
          (json-mode . json-ts-mode)
          (tsx-mode . tsx-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (css-mode . css-ts-mode)
          (html-mode . html-ts-mode))))


(provide 'madmacs-coding-essentials)
