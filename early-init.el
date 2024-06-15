(load (expand-file-name "config/madmacs/madmacs-bootstrap" user-emacs-directory) nil t)

;; Tweak some very basic settings
(setopt madmacs-debug t)
(madmacs--boot :early t)
