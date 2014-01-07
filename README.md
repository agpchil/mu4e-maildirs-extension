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

If the extension has been loaded, simply call `M-x customize-group` and type `mu4e-maildirs-extension`.  Here are a few of the more common customizations. 

### Title

The default label is `Maildirs` but it can be changed with `mu4e-maildirs-extension-title`.

### Position

The variable `mu4e-maildirs-extension-insert-before-str` is used to control where the maildirs summary should be inserted. The valid options are `Basics`, `Bookmarks`, and `Misc`.

### Separators

The left separators `»` and `|` can be changed with `mu4e-maildirs-extension-maildir-separator` and `mu4e-maildirs-extension-submaildir-separator` respectively. 

### Custom list of folders

If you do not want all folders listed, you can specify a custom list of folders using the variable `mu4e-maildirs-extension-custom-list`.

### Faces

You can change the faces of `mu4e-maildirs-extension-maildir-face` and `mu4e-maildirs-extension-maildir-unread-face`. By default this faces inherit from `mu4e`.

### Action text and key

The default action text and key can be changed with `mu4e-maildirs-extension-action-text` and `mu4e-maildirs-extension-action-key`.
If `mu4e-maildirs-extension-action-text` is set to `nil` it won't be displayed.

### Maildirs info

The default format `| maildir_name (unread/total)` can be customized providing your own function. For example, to highlight only the unread count you could use something like this in your `.emacs`:

```lisp
(defun my/mu4e-maildirs-extension-propertize-unread-only (item)
  "Propertize only the maildir unread count using ITEM plist."
  (format "%s\t%s%s %s (%s/%s)\n"
          (if (equal (plist-get item :level) 0) "\n" "")
          (plist-get item :indent)
          (plist-get item :separator)
          (plist-get item :name)
          (propertize (number-to-string (plist-get item :unread))
                      'face (cond
                             ((> (plist-get item :unread) 0) 'mu4e-maildirs-extension-maildir-unread-face)
                             (t            'mu4e-maildirs-extension-maildir-face)))
          (plist-get item :total)))
```

Then set `mu4e-maildirs-extension-propertize-func` to `my/mu4e-maildirs-extension-propertize-unread-only` in the `customize-group` area.
