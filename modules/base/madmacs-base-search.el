;; -*- lexical-binding: t; -*-

(use-package isearch
  :demand t
  :straight nil
  :bind
  (:map minibuffer-local-isearch-map
    ("M-/" . isearch-complete-edit)
   :map occur-mode-map
    ("t" . toggle-truncate-lines)
   :map isearch-mode-map
    ("C-g" . isearch-cancel) ; instead of `isearch-abort'
    ("M-/" . isearch-complete))

  :hook
  (occur-mode . hl-line-mode)
  
  :custom
  (search-whitespace-regexp ".*?") 
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  
  (search-highlight t)
  (isearch-lazy-highlight t)
  (lazy-highlight-initial-delay 0.5)
  (lazy-highlight-no-delay-length 4)
  
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  
  (isearch-wrap-pause t) 
  (isearch-repeat-on-direction-change t)
  (list-matching-lines-jump-to-current-line nil))

(use-package re-builder
  :demand t
  :commands (re-builder regexp-builder)
  :custom
  (reb-re-syntax 'read))

(use-package grep
  :demand t
  :commands (grep lgrep rgrep)
  :custom
  (grep-save-buffers nil)
  (grep-use-headings t) ; Emacs 30

  :config
  (let ((executable (or (executable-find "rg") "grep"))
        (rgp (string-match-p "rg" grep-program)))
    (setq grep-program executable)
    (setq grep-template
          (if rgp
              "/usr/bin/rg -nH --null -e <R> <F>"
            "/usr/bin/grep <X> <C> -nH --null -e <R> <F>"))
    (setq xref-search-program (if rgp 'ripgrep 'grep))))

(use-package
  wgrep
  :after grep
  :bind
  (:map grep-mode-map
    ("e" . wgrep-change-to-wgrep-mode)
    ("C-x C-q" . wgrep-change-to-wgrep-mode)
    ("C-c C-c" . wgrep-finish-edit))
  :custom
  (wgrep-auto-save-buffer t))

(use-package p-search
  :straight (:host github :repo "zkry/p-search")
  :bind
  (:map search-map
    ("p" . p-search)))

(provide 'madmacs-base-search)
