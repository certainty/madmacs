# Madmacs

![img](assets/logo.png)

> This is my Emacs configuration. There are many like it, but this one is mine.

## Usage 

1. clone the repository to ~/.config/emacs
2. run `make deps`
3. start emacs 

## Keybindings

This uses the standard key bindings provided by the various modes.
It builds on top a system in which keys are defined under `C-c <letter>` prefixes which turns `C-c` effectively into a leader key.
In fact, this configuration uses meow mode and leaves the leader pointing to `C-c`.
So you have all those bindings available also in keypad mode under `SPC`.

Most major modes will provide their bindings under `C-c C-<letter>` which is equally easily available.

