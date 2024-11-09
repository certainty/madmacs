;; -*- lexical-binding: t; -*-

(use-package embrace
  :ensure t
  :demand t
  :bind
  (("C-c ca" . embrace-add)
    ("C-c cc" . embrace-change)
    ("C-c cd" . embrace-delete))
  :init
  (add-hook 'markdown-mode-hook
    (lambda ()
      (embrace-add-pair ?_ "_" "_")
      (embrace-add-pair ?i "*" "*")
      (embrace-add-pair ?b "**" "**")))

  (defun embrace-double-quotes ()
    (interactive)
    (embrace--add-internal (region-beginning) (region-end) ?\" ))

  (defun embrace-single-quotes ()
    (interactive)
    (embrace--add-internal (region-beginning) (region-end) ?\')))

(defun meow-setup ()
  "Setup meow keys"
  (interactive)

  (meow-thing-register 'angle
    '(pair (";") (":"))
    '(pair (";") (":")))

  (meow-motion-overwrite-define-key
    '("j" . meow-next)
    '("k" . meow-prev)
    '("h" . meow-left)
    '("l" . meow-right)
    '("`" . capitalize-dwim)
    '("<escape>" . ignore))

  (meow-define-keys 'motion
    '("C-c n" . meow-normal-mode))

  (meow-normal-define-key
    '("0" . meow-expand-0)
    '("9" . meow-expand-9)
    '("8" . meow-expand-8)
    '("7" . meow-expand-7)
    '("6" . meow-expand-6)
    '("5" . meow-expand-5)
    '("4" . meow-expand-4)
    '("3" . meow-expand-3)
    '("2" . meow-expand-2)
    '("1" . meow-expand-1)

    `("X" . "M-x")

    '("!j" . flycheck-next-error)
    '("!k" . flycheck-previous-error)
    '("!," . flycheck-display-error-at-point)
    '("!." . flycheck-explain-error-at-point)

    '("|" . shell-command-on-region)

    '("=" . indent-region)
    '("\\" . indent-rigidly)
    
    '("~" . negative-argument)
    '(";" . meow-reverse)
    '("," . meow-inner-of-thing)
    '("." . meow-bounds-of-thing)
    '("[" . meow-beginning-of-thing)
    '("]" . meow-end-of-thing)

    '(":." . point-to-register)
    '(":," . jump-to-register)
    
    '("`" . capitalize-dwim)
    '("^^" . meow-motion-mode)

    '("&" . meow-query-replace-regexp)
    '("%" . meow-query-replace)

    '("a" . meow-append)
    '("A" . meow-open-below)
    '("b" . meow-back-word)
    '("B" . meow-back-symbol)
    '("c" . meow-change)
    '("d" . meow-delete)
    '("D" . meow-backward-delete)
    '("e" . meow-next-word)
    '("E" . meow-next-symbol)
    '("f" . meow-find)

    ;; g is the quick action key
    '("gg" . meow-visit)
    
    '("gb" . meow-pop-to-mark)
    '("gf" . meow-unpop-to-mark)
    '("gF" . pop-global-mark)
    
    '("gl" . meow-goto-line)
    '("gc" . avy-goto-char)
    '("gC" . avy-goto-char-timer)
    '("g:" . jump-to-register)
    '("gr" . xref-find-references)
    '("gR" . xref-find-references-and-replace)
    '("gd" . xref-find-definitions)
    '("gD" . xref-find-definitions-other-window)
    '("gs". meow-search)

    '("G" . meow-grab)
    '("h" . meow-left)
    '("H" . meow-left-expand)
    '("i" . meow-insert)
    '("I" . meow-open-above)
    '("j" . meow-next)
    '("J" . meow-next-expand)
    '("k" . meow-prev)
    '("K" . meow-prev-expand)
    '("l" . meow-right)
    '("L" . meow-right-expand)

    '("M" . meow-join)
    '("m" . expreg-expand)
    '("n" . expreg-contract)


    '("o" . meow-block)
    '("O" . meow-to-block)
    '("p" . meow-yank)
    '("r" . meow-replace)
    '("R" . meow-swap-grab)
    '("s" . meow-kill)
    '("t" . meow-till)
    '("u" . meow-undo)
    '("U" . meow-undo-in-slection)

    ;; quote
    '("qa" . embrace-add)
    '("qc" . embrace-change)
    '("qd" . embrace-delete)
    '("q\"" . embrace-double-quotes)
    '("q'" .  embrace-single-quotes)
    '("v" . just-one-space)
    '("w" . meow-mark-word)
    '("W" . meow-mark-symbol)
    '("x" . meow-line)
    '("y" . meow-save)
    '("Y" . meow-sync-grab)
    '("'" . meow-pop-selection)
    '("z" . repeat)
    '("Z" . meow-repeat)
    '("<escape>" . meow-cancel-selection)))

;;;; Meow
(use-package meow
  :ensure t
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-use-cursor-position-hack t)
  (meow-use-clipboard t)
  (meow-goto-line-function 'consult-goto-line)

  :config

  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (add-to-list 'meow-mode-state-list '(eshell-mode . insert))
  (add-to-list 'meow-mode-state-list '(mu4e-headers-mode . motion))
  (add-to-list 'meow-mode-state-list '(mu4e-view-mode . motion))
  (add-to-list 'meow-mode-state-list '(helpful-mode . normal))

  (setq meow-use-dynamic-face-color nil)
  (setq meow--kbd-delete-char "<deletechar>")
  (with-eval-after-load 'org
    (modify-syntax-entry ?@ "_" org-mode-syntax-table))

  (meow-setup)
  ;; (meow-setup-indicator)
  (meow-global-mode 1))

(use-package meow-tree-sitter
  :ensure t
  :straight (meow-tree-sitter :type git :host github :repo "skissue/meow-tree-sitter")
  :after meow
  :config
  (meow-tree-sitter-register-defaults))

(provide 'madmacs-keys-meow)

