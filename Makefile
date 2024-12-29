install-emacs:
	brew tap d12frosted/emacs-plus
  brew install emacs-plus@30 --with-native-comp --with-imagemagick --with-modern-black-dragon-icon --with-no-frame-refocus
  cp -r /usr/local/opt/emacs-plus@30/Emacs.app ~/Applications/

deps:
	brew install grip direnv ripgrep fzf fs pandoc graphviz font-iosevka-aile font-iosevka-nerd-font
