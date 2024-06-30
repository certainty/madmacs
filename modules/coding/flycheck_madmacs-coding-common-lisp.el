(use-package sly
  :ensure t
  :hook
  (sly-mode . madmacs--setup-common-lisp)

  :config
  (setopt sly-lisp-implementations
          '((sbcl ("sbcl" "--eval '(ql:quickload :slite/parachute)'") :coding-system utf-8-unix)
            (ros ("ros" "-Q" "run" "-e '(ql:quickload :slite/parachute)'") :coding-system utf-8-unix)
            (qlot+sbcl ("qlot" "exec" "sbcl"))
            (qlot+ros ("qlot" "exec" "ros" "-Q" "run"))))
  (setopt sly-lisp-implementation 'sbcl)
  (setopt inferior-lisp-program "sbcl")
  (setopt sly-complete-symbol-function #'sly-flex-completions)
  (setq sly-contribs '(sly-fancy sly-stickers sly-scratch sly-mrepl sly-autodoc sly-trace-dialog))
  

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

  (defun +lisp/open-repl ()
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

  (defun +lisp/reload-project ()
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
  
  (defvar-keymap madmacs-sly-local-leader-map :doc "Sly related commands")

  ;; keybindings
  (defvar-keymap madmacs-sly-export-map
    :doc "Sly mappings related to exports")
  
  (which-key-add-keymap-based-replacements madmacs-sly-export-map
    "s" '("export symbol at point" . sly-export-symbol-at-point)
    "S" '("export struct" . sly-export-struct)
    "c" '("export class" . sly-export-class))

   (which-key-add-keymap-based-replacements madmacs-sly-local-leader-map
     "," '("Sly" . sly)
     "e" '("Export" . madmacs-sly-export-map))

  (evil-define-key 'normal sly-mode-map (kbd "<localleader>") madmacs-sly-local-leader-map)
  
   )


(use-package sly-asdf
  :ensure t
  :after sly
  :config
  (defvar-keymap madmacs-sly-asdf-map :doc "Sly asdf keys")
  
  (which-key-add-keymap-based-replacements madmacs-sly-asdf-map
    "a" '("ASDF reload system" . sly-asdf-reload-system)
    "c" '("ASDF compile system" . sly-asdf-compile-system)
    "l" '("ASDF load system" . sly-asdf-load-system)
    "d" '("ASDF browse system" . sly-asdf-browse-system)
    "t" '("ASDF test system" . sly-asdf-test-system))

  (which-key-add-keymap-based-replacements madmacs-sly-local-leader-map
    "a" `("ASDF" . ,madmacs-sly-asdf-map))
  
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
