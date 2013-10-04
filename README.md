# Mu4e maildirs extension

This extension adds a maildir summary in `mu4e-main-view`.

It gets the list of maildirs and runs a `mu` command for each maildir to count unread and total mails.

To minimize performance issues this information is _cached_ until the index is changed (using `mu4e-index-updated-hook`) and its not rebuilt until `mu4e-main-view` is called (or if `mu4e-main-view` is `current-buffer`). Also, the cache can be updated by pressing `u` in `mu4e-main-view` or updating the index with `U`.

![Screenshot](https://drive.google.com/uc?export=view&id=0Byv-S6nIE7oRVm85UGVxY3FqMUE)

## Requirements
This extension needs [mu4e](http://github.com/djcb/mu) version 0.9.9.5 or newer to work.

## Installation
It's available on [MELPA](http://melpa.milkbox.net).
```
M-x package-install mu4e-maildirs-extension
(mu4e-maildirs-extension)
```

Or you can copy `mu4e-maildirs-extension.el` file in your load path and add the following in your `~/.emacs` startup file:
```lisp
(require 'mu4e-maildirs-extension)
(mu4e-maildirs-extension)
```

## Directory structure

This extension expects the following maildir structure in `mu4e-maildir` directory:

```
account1/
  submaildir1/
  submaildir2/
  ...
account2/
  submaildir1/
  submaildir2/
  ...
```

Pop3 configurations usually have `{cur,new,tmp}` directly in `account1/` but you should put them inside a submaildir to make this extension work (ex: `account1/inbox/{cur,new,tmp}`).

## Customize

### Title

The default label is `Maildirs` but it can be changed with:

```lisp
(setq mu4e-maildirs-extension-title "  Maildirs\n"
```

### Position

The variable `mu4e-maildirs-extension-insert-before-str` is used to control where the maildirs summary should be inserted. It can be changed with:

```lisp
(setq mu4e-maildirs-extension-insert-before-str "\n  Misc")
```

### Separators

The left separators `»` and `|` can be changed with:

```lisp
(setq mu4e-maildirs-extension-maildir-separator "\n\t» ")
(setq mu4e-maildirs-extension-submaildir-separator "\t  | ")
```