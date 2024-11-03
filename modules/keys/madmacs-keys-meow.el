;; -*- lexical-binding: t; -*-

(defun meow-setup ()
  "Setup meow keys"
  (interactive)
  
  (meow-thing-register 'angle
    '(pair (";") (":"))
    '(pair (";") (":")))
  
  (meow-leader-define-key
    ;; Use SPC (0-9) for digit arguments.
    '("1" . meow-digit-argument)
    '("2" . meow-digit-argument)
    '("3" . meow-digit-argument)
    '("4" . meow-digit-argument)
    '("5" . meow-digit-argument)
    '("6" . meow-digit-argument)
    '("7" . meow-digit-argument)
    '("8" . meow-digit-argument)
    '("9" . meow-digit-argument)
    '("0" . meow-digit-argument)
    '("-" . meow-keypad-describe-key)
    '("_" . meow-cheatsheet))
  
  (meow-motion-overwrite-define-key
    '("j" . meow-next)
    '("k" . meow-prev)
    '("h" . meow-left)
    '("l" . meow-right)
    '("`" . capitalize-dwim)
    '("<escape>" . ignore))

  (meow-define-keys 'motion
    '("C-M-s-^" . meow-normal-mode))

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
    
    '(">" . xref-find-definitions)
    '("<" . xref-find-references)
    '("^" . meow-motion-mode)
    '("-" . negative-argument)
    '(";" . meow-reverse)
    '("," . meow-inner-of-thing)
    '("." . meow-bounds-of-thing)
    '("[" . meow-beginning-of-thing)
    '("]" . meow-end-of-thing)
    '(":" . consult-goto-line)
    '("`" . capitalize-dwim)
    
    '("a" . meow-append)
    '("A" . meow-open-below)
    '("b" . meow-back-word)
    '("B" . meow-back-symbol)
    `("c" . ,mode-specific-map)
    '("C" . meow-change)
    '("d" . meow-delete)
    '("D" . meow-backward-delete)
    '("e" . meow-next-word)
    '("E" . meow-next-symbol)
    '("f" . meow-find)
    '("g" . meow-line)
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
    '("m" . pop-global-mark)
    '("M" . meow-join)
    '("n" . avy-goto-char-timer)
    '("N".  avy-goto-char)
    '("o" . meow-block)
    '("O" . meow-to-block)
    '("p" . meow-yank)
    '("r" . meow-replace)
    '("R" . meow-swap-grab)
    '("s" . meow-kill)
    '("t" . meow-till)
    '("u" . meow-undo)
    '("U" . meow-undo-in-selection)
    '("v" . expreg-expand)              ; enter rectangle mark mode
    '("V" . expreg-contract)
    '("w" . meow-mark-word)
    `("x" . ,ctl-x-map)
    '("W" . meow-mark-symbol)
    '("y" . meow-save)
    '("Y" . meow-sync-grab)
    '("'" . meow-pop-selection)
    '("z" . meow-repeat)
    '("&" . meow-query-replace-regexp)
    '("%" . meow-query-replace)
    '("/" . meow-visit)
    '("<escape>" . meow-cancel-selection)))

;;;; Meow
(use-package meow
  :ensure t
  :after (which-key moody)
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-use-cursor-position-hack t)
  (meow-use-clipboard t)
  (meow-goto-line-function 'consult-goto-line)

  :config
  (add-to-list 'meow-char-thing-table '(?a . angle))
  
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
  (meow-global-mode 1))

(provide 'madmacs-keys-meow)

