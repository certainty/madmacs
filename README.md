# Madmacs

![img](assets/logo.png)

> This is my Emacs configuration. There are many like it, but this one is mine.

## Madmacs in a few bullet points 
* Work in progress
  * build by me for me
  * not a framework but feel free to take it as a start for your own version
* Built on the shoulders of giants
  * many parts are based on other configurations like doom, lambda-emacs, centaur-emacs, etc.
  * it's slightly nuts that we have to go to that length to configure our editor 
* Optimized for software engineers
  * I use this as my daily driver 
* Configuration in plain emacs lisp
  * `use-package` with straight
  * simple boot approach that's easy to follow and modify
* Vanilla emacs + meow
  * Consistent keybinding scheme using `C-c` as prefix
  * `C-c z` is mode specific prefix
  * meow for qwerty, which can be disabled
* Useful power-ups
  * Avy + Embark
  * Hydras
  * Vetico + Corfu + Marginalia & the gang 
* Fairly fast startup
  * I don't obsess over it. I don't know about you, but I do not restart my emacs all the time.
  

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

For convenience some of those are re-bound under the `C-c z` mode specific prefix.

Avy & Embark are available in buffer via `C-:` for avy and `C-.` and `M-.` for embark.

If you're lost just hit `C-c` and let which-key guide you.

