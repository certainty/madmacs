
(use-package subr-x
  :ensure nil
  :straight nil
  :defer t)

(use-package cl-lib
  :ensure nil
  :straight nil
  :defer t)

(use-package async
  :ensure t
  :config
  (setopt dired-async--modeline-mode nil))

(provide 'madmacs-libraries)
