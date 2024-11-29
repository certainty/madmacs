# Madmacs

![img](assets/logo.png)

> This is my Emacs configuration. There are many like it, but this one is mine.

## For programmers: Eglot + Copilot/Gptel + Modern Power Ups

This is my emacs configuration, made for me as a software engineer.
I use meow for modal editing, eglot as my LSP client and I use modern power ups to support my workflow:

* Vertico + Corfu + Marginalia for completion and choices
* Embark for contextual actions
* Dirvish for file management and project tree
* Copilot / Gptel to bring AI to my work


## My Emacs Configuration Approach

I use plain Emacs Lisp for my configuration. To manage packages, I employ `use-package` alongside `straight`. This setup provides a straightforward boot process that's easy to understand and modify.

For modal editing, I prefer Meow. It offers a consistent keybinding scheme `C-c` as prefixes, while `C-c m` serves as a mode-specific prefix.

While my setup starts fairly quickly, I don't focus heavily on startup time. Personally, I do not restart Emacs frequently, so startup speed isn't a primary concern for me.

## Usage

1. clone the repository to ~/.config/emacs
2. run `make deps`
3. start emacs

## Keybindings

There are a couple of very basic conventions that I have applied.

1. Global actions, actions that don't work on text or region are bound in the leader map under `C-c`. You'll fine a mnemonic grouping.
2. Mode specific global actions are made available under the local leader map `C-c m`
3. Context specific actions (on point, or region, etc.) are usually made available via embark actions. 


**Transient menus where possible**

Use `M-o` in any mode that is supported by [casual suite](https://github.com/kickingvegas/casual-suite) to fire up the transient menu.

**The rest is just Emacs**

For the rest just use the normal emacs keybindings.

## Usage of LLM / AI to assist programming

I've enabled and configured GitHub Copilot and GPTel, ready to be used from within Emacs. Copilot is used for auto-completion of code, while GPTel is made available to interact in any buffer.

All AI related functionality is available via `C-c ,` and `C-c .` prefixes.

In addition, there are `embark` actions available for `gptel`, so you can use `C-.` to access them in the given context.

## Environment

When you use the GUI Emacs on MacOS, which is me, you have to make sure that the environment is setup correctly.
In particular you want your PATH setup so that everything is in place. Otherwise you may end up with native compilation problems
because the proper paths can't be found. On top of that it's just handy to be able to set the environment explicitly.

If you want to do that just place a file named `madmacs.env` in your emacs directory.
This will be read on startup and madmacs sets the environment from it. It will also update the `exec-path`.

The content of the file is a simplified env file, which you can obtain by running `env > ~/.config/emacs/madmacs.env`.
Please double check that you don't have any secrets in this file.


## Screenshots

<img src="https://github.com/certainty/madmacs/assets/338957/d74304b2-3a2f-4f71-949e-a23bff07e17a" width=400px>

<img src="https://github.com/user-attachments/assets/b2733cec-9e48-4052-a6a8-ea6ddfbf3bed" width=400px>

<img src="https://github.com/certainty/madmacs/assets/338957/77bc63dd-93dc-4f75-a81d-75327168c246" width=400px>

<img src="https://github.com/user-attachments/assets/dc95b831-14bb-4f49-8564-9df4be1a998e" width=400px>

<img src="https://github.com/user-attachments/assets/fa88abbf-57d0-4907-a965-21567a985ec7" width=400px>

<img src="https://github.com/user-attachments/assets/fd6d0af5-ede0-4aa6-a6de-fcd477534374" width=400px>


<img src="https://github.com/user-attachments/assets/9f2b07fe-0980-43e0-b416-7363334ec6b2" width=400px>

