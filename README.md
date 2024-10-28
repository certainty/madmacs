# Madmacs

![img](assets/logo.png)

> This is my Emacs configuration. There are many like it, but this one is mine.

## Madmacs in a few bullet points

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
  * Transient menues via casual (suite)
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

Use `M-o` in any mode that is supported by [casual suite](https://github.com/kickingvegas/casual-suite) to fire up the transient menu.


## Usage of LLM / AI to assist programming

I've enabled and configured GitHub Copilot and GPTel, ready to be used from within Emacs. Copilot is used for auto-completion of code, while GPTel is made available to interact in any buffer.

All AI related functionality is available via `SPC a` prefix.

In addition, there are `embark` actions available for `gptel`, so you can use `C-.` to access them in the given context.

## Environment

When you use the GUI Emacs on MacOS, which is me, you have to make sure that the environment is setup correctly.
In particular you want your PATH setup so that everything is in place. Otherwise you may end up with native compilation problems
because the proper paths can't be found. On top of that it's just handy to be able to set the environment explicitly.

If you want to do that just place a file named `madmacs.env` in your emacs directory.
This will be read on startup and madmacs sets the environment from it. It will also update the `exec-path`.

The content of the file is a simplified env file, which you can obtain by running `env > ~/.config/emacs/madmacs.env`.
Please double check that you don't have any secrets in this file.


## SCREENSHOTS
<img src="https://github.com/certainty/madmacs/assets/338957/d74304b2-3a2f-4f71-949e-a23bff07e17a" width=400px>

<img src="https://github.com/certainty/madmacs/assets/338957/ad5a5eda-d22e-41e7-9be0-e667b3c1c8d3" width=400px>

<img src="https://github.com/certainty/madmacs/assets/338957/77bc63dd-93dc-4f75-a81d-75327168c246" width=400px>

<img src="https://github.com/certainty/madmacs/assets/338957/478b536b-28c6-4572-a203-5aa9f7bf030b" width=400px>



