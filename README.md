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
  * Uses eglot as LSP client
* Configuration in plain emacs lisp
  * `use-package` with straight
  * simple boot approach that's easy to follow and modify
* Evil emacs (I just can't get out of modal editing)
  * Consistent keybinding scheme using `SPC` as prefix
  * `,` is mode specific prefix
* Useful power-ups
  * Avy + Embark
  * Transient via casual (suite)
  * Vetico + Corfu + Marginalia & the gang
  * expand-region
  * iedit for multiple cursor edit
* Fairly fast startup
  * I don't obsess over it. I don't know about you, but I do not restart my emacs all the time.


## Usage

1. clone the repository to ~/.config/emacs
2. run `make deps`
3. start emacs

## Keybindings

The global keybinds are available in the leader map via `SPC`.
Mode specific leader bindings are available (if they exist) under the local leader `,`.


## Screenshots
<img src="https://github.com/certainty/madmacs/assets/338957/d74304b2-3a2f-4f71-949e-a23bff07e17a" width=400px>

<img src="https://github.com/certainty/madmacs/assets/338957/ad5a5eda-d22e-41e7-9be0-e667b3c1c8d3" width=400px>


<img src="https://github.com/certainty/madmacs/assets/338957/77bc63dd-93dc-4f75-a81d-75327168c246" width=400px>



<img src="https://github.com/certainty/madmacs/assets/338957/478b536b-28c6-4572-a203-5aa9f7bf030b" width=400px>



