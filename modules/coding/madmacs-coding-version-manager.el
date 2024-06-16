
(use-package asdf
  :ensure t
  :straight (asdf :type git :host github :repo "tabfugnic/asdf.el")
  :config
  (asdf-enable))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(provide 'madmacs-coding-version-manager)
