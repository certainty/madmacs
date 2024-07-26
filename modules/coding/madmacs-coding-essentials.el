;; -*- lexical-binding: t; -*-

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

(use-package fancy-compilation
    :ensure t
    :commands (fancy-compilation-mode)
    :init
    (with-eval-after-load 'compile
      (fancy-compilation-mode)))

(use-package emacs
  :ensure nil
  :straight nil
  :init
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'electric-pair-mode)

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
                 (side . bottom)
                 (slot . 5))))

(provide 'madmacs-coding-essentials)
