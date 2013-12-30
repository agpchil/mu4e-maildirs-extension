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

### Custom list of folders

If you do not want all folders listed, you can specify a custom list of folders using the variable `mu4e-maildirs-extension-custom-list`.

```lisp
(setq mu4e-maildirs-extension-custom-list
  '( "/account1/INBOX" "/account2/INBOX" ))
```

### Faces

You can change the faces of `mu4e-maildirs-extension-maildir-face` and `mu4e-maildirs-extension-maildir-unread-face`. By default this faces inherit from `mu4e`.

### Action text and key

The default action text and key can be changed with:

```lisp
(setq mu4e-maildirs-extension-action-text "\t* [u]pdate index & cache\n")
(setq mu4e-maildirs-extension-action-key "u")
```

### Maildirs info

The default format `| maildir_name (unread/total)` can be customized providing your own function. For example, to highlight only the unread count you could use something like this:

```lisp
(defun my/mu4e-maildirs-extension-propertize-unread-only (separator name unread total)
  (format "%s%s (%s/%s)\n"
          separator
          name
          (propertize (number-to-string unread)
                      'face
                      (cond ((> unread 0)
                             'mu4e-maildirs-extension-maildir-unread-face)
                            (t
                             'mu4e-maildirs-extension-maildir-face)))
          total))

(setq mu4e-maildirs-extension-propertize-func 'my/mu4e-maildirs-extension-propertize-unread-only)
```

### Single Account Mode

If you only have one account in your `mu4e-maildir` directory, you can set single account mode on:

```lisp
(setq mu4e-maildirs-extension-single-account-mode t)
```

Now custom folders, set via `mu4e-maildirs-extension-custom-list`, can be referenced without an account qualification as:

```lisp
(setq mu4e-maildirs-extension-custom-list
      '("/INBOX" 
        "/[Gmail].All Mail" 
        "/[Gmail].Starred" 
        "/[Gmail].Important" ))
```

Setting `(setq mu4e-maildirs-extension-submaildir-separator "\t+ ")` will render the folders without indent as:

```
	* [u]pdate index & cache
	+ /INBOX (14/82)
	+ /[Gmail].All Mail (16/9074)
	+ /[Gmail].Starred (0/1)
	+ /[Gmail].Important (6/35)
```
