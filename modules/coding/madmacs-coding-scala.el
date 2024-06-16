(use-package scala-mode
  :ensure t
  :interpreter ("scala" . scala-mode))

(use-package lsp-metals
  :ensure t
  :hook (scala-mode . lsp))

(use-package sbt-mode
  :ensure t
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   (setq sbt:program-options '("-Dsbt.supershell=false")))


(provide 'madmacs-coding-scala)
