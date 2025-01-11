;; -*- lexical-binding: t; -*-

;; remove superfluous UI elements
(menu-bar-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode -1)
(require 'cl-lib)

;;(setopt use-package-compute-statistics t) 

(when (display-graphic-p)
;; load the environment file if it exists
  (let ((env-file (expand-file-name "madmacs.env" user-emacs-directory)))
    (when (file-exists-p env-file)
      (message ";; Loading environment variables from %s" env-file)
      (with-temp-buffer
        (insert-file-contents env-file)
        (while (not (eobp))
          (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
            (when (string-match "\\([^=]+\\)=\"?\\(.*\\)\"?$" line)
              (let ((variable (match-string 1 line))
                     (value (match-string 2 line)))
                (setenv variable value)
                (when (string= variable "PATH")
                  (message ";; Adding content of PATH to exec-path" value)
                  (dolist (value (split-string value ":"))
                    (cl-pushnew value exec-path :test #'string=)))))
            (forward-line 1)))))))
