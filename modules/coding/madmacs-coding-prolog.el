
(use-package sweeprolog
  :ensure t
  :straight (sweeprolog :type git :host github :repo "SWI-Prolog/packages-sweep" :files ("*.el" "*.pl" ".texi"))
  :hook
  (sweeprolog-mode . sweeprolog-electric-layout-mode)

  :custom
  (sweeprolog-swipl-path "/opt/homebrew/bin/swipl")

  :config
  (add-to-list 'auto-mode-alist '("\\.plt\\'" . sweeprolog-mode))
  (add-to-list 'auto-mode-alist '("\\.pl\\'" . sweeprolog-mode))
  (add-to-list 'auto-mode-alist '("\\.pro\\'" . sweeprolog-mode))
  (add-to-list 'auto-mode-alist '("\\.swipl\\'" . sweeprolog-mode)))



(provide 'madmacs-coding-prolog)
