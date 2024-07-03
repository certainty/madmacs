;; -*- lexical-binding: t; -*-

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :custom
  (markdown-command "pandoc -f markdown -t html"))

(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'")

(provide 'madmacs-writing-essentials)
