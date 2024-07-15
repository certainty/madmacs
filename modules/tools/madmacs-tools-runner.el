;; -*- lexical-binding: t; -*-

(use-package cli2eli
  :ensure t
  :straight (:host github :repo "nohzafk/cli2eli" :branch "main")
  :config
  (cli2eli-load-tool
   (expand-file-name "quick-commands.json" user-emacs-directory)))

(provide 'madmacs-tools-runner)
