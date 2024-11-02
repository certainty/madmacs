;; -*- lexical-binding: t; -*-

(defvar-keymap madmacs-sly-local-leader-keys :doc "Sly related commands")
(defvar-keymap madmacs-sly-asdf-keys :doc "Sly asdf keys")

(use-package sly
  :ensure t
                                        ;:hook
                                        ;(sly-mode . madmacs--setup-common-lisp)

  :custom
  (sly-lisp-implementation 'sbcl)
  (inferior-lisp-program "sbcl")
  (sly-complete-symbol-function #'sly-flex-completions)

  :init
  (add-to-list 'auto-mode-alist '("\\.ros\\'" . lisp-mode))

  :config
  (setopt sly-lisp-implementations
    `((sbcl+prelude ("sbcl" "--eval" "(ql:quickload :cl-dk-prelude)") :coding-system utf-8-unix)
       (sbcl+pure ("sbcl") :coding-system utf-8-unix)
       (sbcl+scripting ("sbcl" "--core" ,(expand-file-name "~/common-lisp/cl-dk-prelude-scripting.core") "--eval" "(in-package :cl-dk-prelude-scripting)") :coding-system utf-8-unix)
       (ros ("ros" "-Q" "run" "-e '(ql:quickload :slite/parachute)'") :coding-system utf-8-unix)
       (qlot+sbcl ("qlot" "exec" "sbcl"))
       (qlot+ros ("qlot" "exec" "ros" "-Q" "run"))))

  (setq sly-contribs '(sly-fancy sly-stickers sly-scratch sly-mrepl sly-autodoc sly-trace-dialog))
  (setopt sly-common-lisp-set-style 'modern)
  

  (add-to-list 'display-buffer-alist
    '("^\\*sly-compilation"
       (display-buffer-reuse-window
         display-buffer-in-side-window)
       (side . bottom)
       (reusable-frames . visible)
       (window-height . 0.3)))

  (add-to-list 'display-buffer-alist
    '("^\\*sly-mrepl"
       (display-buffer-reuse-window
         display-buffer-in-side-window)
       (side . right)
       (reusable-frames . visible)
       (window-height . 0.4)))


  (defvar inferior-lisp-program "sbcl")

  (defun madmacs--select-implementation ()
    "Select the Common Lisp implementation to use."
    (interactive)
    (let ((current-prefix-arg '-))
      (call-interactively #'sly)))

  (defun madmacs--open-repl ()
    "Open the Sly REPL."
    (interactive)
    (require 'sly)
    (if (sly-connected-p)
      (sly-mrepl)
      (sly nil nil t)
      (cl-labels ((recurse (attempt)
                    (sleep-for 1)
                    (cond ((sly-connected-p) (sly-mrepl))
                      ((> attempt 5) (error "Failed to start Slynk process."))
                      (t (recurse (1+ attempt))))))
        (recurse 1))))

  (defun madmacs--reload-project ()
    "Restart the Sly session and reload a chosen system."
    (interactive)
    (sly-restart-inferior-lisp)
    (cl-labels ((recurse (attempt)
                  (sleep-for 1)
                  (condition-case nil
                    (sly-eval "PONG")
                    (error (if (= 5 attempt)
                             (error "Failed to reload Lisp project in 5 attempts.")
                             (recurse (1+ attempt)))))))
      (recurse 1)
      (sly-asdf-load-system)))

  (defun madmacs--setup-common-lisp ()
    (if (file-exists-p "~/.local/share/quicklisp/log4sly-setup.el")
      (progn
        (load "~/.local/share/quicklisp/log4sly-setup.el")
        (global-log4sly-mode 1))
      (message "log4sly not found")))

  ;; keybindings
  (defvar-keymap madmacs-sly-export-map :doc "Sly mappings related to exports")
  (which-key-add-keymap-based-replacements madmacs-sly-export-map
    "s" '("export symbol at point" . sly-export-symbol-at-point)
    "S" '("export struct" . sly-export-struct)
    "c" '("export class" . sly-export-class))

  (defvar-keymap madmacs-sly-diagnostics-map :doc "Sly mappings related to diagnostics")
  (which-key-add-keymap-based-replacements madmacs-sly-diagnostics-map
    "!" '("next note" . sly-next-note)
    "c" '("clear notes" . sly-remove-notes)
    "n" '("next note" . sly-next-note)
    "e" '("next error" . sly-next-error))

  (defvar-keymap madmacs-sly-compile-keys :doc "Sly mappings related to compilation")
  (which-key-add-keymap-based-replacements madmacs-sly-compile-keys
    "c" '("function" . sly-compile-defun)
    "f" '("file" . sly-compile-file)
    "F" '("and load file" . sly-compile-and-load-file)
    "r" '("region" . sly-compile-region)
    "l" '("load" . sly-load-file))

  (defvar-keymap madmacs-sly-help-keys :doc "Sly mappings related to help")
  (which-key-add-keymap-based-replacements madmacs-sly-help-keys
    "." '("help" . sly-describe-symbol)
    "H" '("hyperspec lookup" . hyperspec-lookup)
    "~" '("hyperspec format" . hyperspec-lookup-format)
    "#" '("hyperspec reader macro" . hyperspec-lookup-reader-macro)
    "d" '("documentation" . sly-documentation-lookup))

  (defvar-keymap madmacs-sly-xref-keys :doc "Sly mappings related to xref")
  (which-key-add-keymap-based-replacements madmacs-sly-xref-keys
    "a" '("apropos" . sly-apropos)
    "p" '("apropos package" . sly-apropos-package)
    "<" '("who calls" . sly-who-calls)
    ">" '("calls who" . sly-calls-who)
    "r" '("who reference" . sly-who-references)
    "b" '("who binds" . sly-who-binds)
    "m" '("who expands" . sly-who-macroexpands)
    "S" '("who sets" . sly-who-sets)
    "s" '("who specializes" . sly-who-specializes))

  (defvar-keymap madmacs-sly-repl-keys :doc "Sly mappings related to repl")
  (which-key-add-keymap-based-replacements madmacs-sly-repl-keys
    "c" '("clear" . sly-mrepl-clear-repl)
    "l" '("load system" . sly-asdf-load-system)
    "r" '("restart" . sly-restart-inferior-lisp)
    "R" '("reload project" . madmacs--reload-project)
    "s" '("sync" . sly-mrepl-sync))

  (defvar-keymap madmacs-sly-stickers-keys :doc "Sly mappings related to stickers")
  (which-key-add-keymap-based-replacements madmacs-sly-stickers-keys
    "b" '("toggle break points" . sly-stickers-toggle-break-on-stickers)
    "c" '("clear defun" . sly-stickers-clear-defun-stickers)
    "C" '("clear buffer" . sly-stickers-clear-buffer-stickers)
    "f" '("fetch" . sly-stickers-fetch)
    "s" '("add/remove" . sly-stickers-dwim))

  (defvar-keymap madmacs-sly-trace-keys :doc "Sly mappings related to trace")
  (which-key-add-keymap-based-replacements madmacs-sly-trace-keys
    "t" '("toggle" . sly-trace-dialog-toggle-trace)
    "T" '("toggle (complex)" . sly-trace-dialog-toggle-complex-trace)
    "d" '("dialog" . sly-trace-dialog)
    "u" '("untrace all" . sly-trace-dialog-untrace-all))
  
  (which-key-add-keymap-based-replacements madmacs-sly-local-leader-keys
    "," '("Sly" . madmacs--open-repl)
    ";" '("Sly select" . madmacs--select-implementation)
    "!" `("Diagnostics" . ,madmacs-sly-diagnostics-map)
    "l" '("Quickload" . sly-quickload)
    "D" '("Disassemble" . sly-disassemble-symbol)
    "e" `("Export" . ,madmacs-sly-export-map)
    "a" `("ASDF" . ,madmacs-sly-asdf-keys)
    "c" `("Compile" . ,madmacs-sly-compile-keys)
    "d" `("Docs" . ,madmacs-sly-help-keys)
    "r" `("Repl" . ,madmacs-sly-repl-keys)
    "s" `("Stickers" . ,madmacs-sly-stickers-keys)
    "t" `("Trace" . ,madmacs-sly-trace-keys)
    "m" '("expand macro" . macrostep-expand)
    "x" `("Xref" . ,madmacs-sly-xref-keys))

  (cl-case madmacs-modal-approach
    (evil
     (evil-define-key 'normal sly-mode-map (kbd "<localleader>") madmacs-sly-local-leader-keys)
     (evil-define-key 'visual sly-mode-map (kbd "<localleader>") madmacs-sly-local-leader-keys))
    (meow
     (define-key sly-mode-map (kbd "C-,") madmacs-sly-local-leader-keys))
    (boon
      (define-key sly-mode-map (kbd "C-c ,") madmacs-sly-local-leader-keys))))


(use-package sly-asdf
  :ensure t
  :after sly
  :config
  
  (which-key-add-keymap-based-replacements madmacs-sly-asdf-keys
    "a" '("ASDF reload system" . sly-asdf-reload-system)
    "c" '("ASDF compile system" . sly-asdf-compile-system)
    "l" '("ASDF load system" . sly-asdf-load-system)
    "d" '("ASDF browse system" . sly-asdf-browse-system)
    "t" '("ASDF test system" . sly-asdf-test-system))
  
  :init
  (add-to-list 'sly-contribs 'sly-asdf 'append))

(use-package sly-quicklisp
  :ensure t
  :hook sly-mode)

(use-package sly-macrostep
  :ensure t)

(use-package sly-repl-ansi-color
  :ensure t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(use-package sly-overlay
  :ensure t)

(provide 'madmacs-coding-common-lisp)
