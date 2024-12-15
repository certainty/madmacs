;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :demand t
  :straight nil
  :bind
  (:map madmacs-mode-map
	("C-x f" . find-file)
	("C-g" . prot/keyboard-quit-dwim))

  
  :custom
  (sentence-end-double-space nil)
  (tab-width 2)
  (fill-column 150)
  (tab-always-indent 'complete)
  (completion-cycle-threshold 3)
  (require-final-newline nil)
  (visible-bell nil)
  (use-short-answers t)
  (undo-limit 67108864)
  (undo-strong-limit 100663296)
  (use-short-answers t)
  (blink-cursor-mode 0)
  (indent-tabs-mode nil)
  
  :init
  (defun prot/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package frame
  :demand t
  :straight nil
  :hook (before-make-frame . window-divider-mode)
  :bind
  (:map madmacs-keymap-ux
    ("w" . toggle-frame-fullscreen))
  
  :custom
  (confirm-kill-emacs nil)
  (default-frame-alist
   '((frame-title-format . nil)
     (internal-border-width . 2)
     (tool-bar-lines . 0)
     (vertical-scroll-bars . nil)
     (horizontal-scroll-bars . nil)))
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only))

(use-package transpose-frame
  :bind
  (:map madmacs-mode-map
    ("C-x w t" . transpose-frame)
    ("C-x w r" . rotate-frame-clockwise)
    ("C-x w f" . flip-frame)
    ("C-x w F" . flop-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package window
  :demand t
  :straight nil
  :hook
  (after-init . winner-mode)
  :bind
  (:map madmacs-mode-map 
	("M-o" . other-window)
	("C-x {" . shrink-window)
	("C-x }" . enlarge-window)
	("C-x >" . enlarge-window-horizontally)
	("C-x <" . shrink-window-horizontally)
	("C-x w +" . balance-windows)
	("C-x w =" . balance-windows-area))
  (:repeat-map resize-window-repeat-map
	       (">" . enlarge-window-horizontally)
	       ("<" . shrink-window-horizontally))
  :custom
  ;; I have a wide-screen so I prefer vertical splits
  (split-width-threshold 0)
  (split-height-threshold nil)
  
  (display-buffer-base-action nil))

(use-package ace-window
  :demand t
  :bind
  (:map madmacs-mode-map 
	("C-x w w" . ace-window)
	("C-x w x" . ace-swap-window)
	("C-x w m" . ace-maximize-window)
  (:map goto-map
	("w" . ace-window))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package so-long
  :straight nil
  :hook (after-init . global-so-long-mode))

(use-package simple
  :straight nil
  :hook (after-init . global-visual-line-mode)
  :custom
  (kill-whole-line t)
  (line-move-visual t))


(use-package display-line-numbers
  :straight nil
  :hook (after-init . global-display-line-numbers-mode)
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width-start t))

(use-package emacs
  :demand t
  :custom
  (auto-save-default nil)
  (auto-window-vscroll nil)
  (scroll-step 1)
  (scroll-margin 3)
  (scroll-conservatively 101)
  (scroll-up-aggressively 0.01)
  (scroll-down-aggressively 0.01)
  (fast-but-imprecise-scrolling nil)
  (hscroll-step 1)
  (hscroll-margin 1)
  (switch-to-buffer-preserve-window-point t)
  
  :config
  (add-to-list 'display-buffer-alist
    '((or . ((derived-mode . flymake-diagnostics-buffer-mode)
              (derived-mode . flymake-project-diagnostics-mode)
              (derived-mode . messages-buffer-mode)
              (derived-mode . backtrace-mode)))
       (display-buffer-reuse-mode-window display-buffer-at-bottom)
       (window-height . 0.3)
       (dedicated . t)
       (preserve-size . (t . t))))

  (add-to-list 'display-buffer-alist
    '("\\*\\(Output\\|Register Preview\\).*"
       (display-buffer-reuse-mode-window display-buffer-at-bottom)))

  (add-to-list 'display-buffer-alist
    '("\\`\\*Async Shell Command\\*\\'"
      (display-buffer-no-window))))

(use-package uniquify
  :demand t
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package whitespace
  :demand t
  :bind
  (:map madmacs-keymap-ux
	("W" . whitespace-mode))
  (:map madmacs-mode-map
	("C-c z" . delete-trailing-whitespace)))

(use-package revert-buffer-all)

(use-package popper
  :bind
  (:map madmacs-mode-map
        ("C-`"  . popper-toggle)
        ("M-`"  . popper-cycle)
        ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
    '("\\*Messages\\*"
       "Output\\*$"
       "\\*Async Shell Command\\*"
       copilot-chat-mode
       sly-mrepl-mode
       help-mode
       compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package savehist
  :straight nil
  :hook (after-init . savehist-mode)
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 100))

(use-package helpful
  :bind
  (:map madmacs-mode-map
	("C-h f" . helpful-callable)
	("C-h v" . helpful-variable)
	("C-h k" . helpful-key)
	("C-h x" . helpful-command)
	("C-c h" . helpful-at-point)))



(provide 'madmacs-base-display-elements)
