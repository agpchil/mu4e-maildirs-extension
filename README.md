# Mu4e maildirs extension

This extension adds a maildir summary in `mu4e-main-view`.

It gets the list of maildirs and runs a `mu` command for each maildir to count unread and total mails.

To minimize performance issues this information is _cached_ until the index is changed (using `mu4e-index-updated-hook`) and its not rebuilt until `mu4e-main-view` is called (or if `mu4e-main-view` is `current-buffer`). Also, the cache can be updated by pressing `u` in `mu4e-main-view` or updating the index with `U`.

![Screenshot](https://drive.google.com/uc?export=view&id=0Byv-S6nIE7oRVm85UGVxY3FqMUE)

## Basic Usage

```lisp
(require 'mu4e-maildirs-extension)
(mu4e-maildirs-extension)
```

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