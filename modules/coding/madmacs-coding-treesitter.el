;; -*- lexical-binding: t; -*-

(use-package treesit-auto
    :ensure t
    :custom
    (treesit-font-lock-level 4) ;; make sure we get the elements highlighted we're interested in
    (treesit-auto-install 'prompt)
    (treesit-language-source-alist
     '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
       (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp"))
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
      (bash "https://github.com/tree-sitter/tree-sitter-bash/"))
    (treesit-auto-langs '(ruby scala rust typescript javascript yaml dockerfile json html css lua elisp)) ; don't include go and gomod currently until I figure out how to change the tab-width
    :config
    (global-treesit-auto-mode))

(provide 'madmacs-coding-treesitter)
