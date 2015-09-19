# Mu4e maildirs extension

This extension adds a maildir summary in `mu4e-main-view`.

It gets the list of maildirs and runs a `mu` command async for each maildir to count unread and total mails. To minimize performance issues this information is _cached_.


![Screenshot](https://drive.google.com/uc?export=view&id=0Byv-S6nIE7oRVm85UGVxY3FqMUE)

## Requirements
This extension needs [mu4e](http://github.com/djcb/mu) version 0.9.9.5 or newer to work.

## Installation
It's available on [MELPA](http://melpa.milkbox.net).
```lisp
M-x package-install mu4e-maildirs-extension
(mu4e-maildirs-extension)
```

Or you can copy `mu4e-maildirs-extension.el` file in your load path and add the following in your `~/.emacs` startup file:
```lisp
(require 'mu4e-maildirs-extension)
(mu4e-maildirs-extension)
```

## Keybindings

The following key-bindings are added in `mu4e-main-view`:
- `u` Update the index
- `C-u u` only clear the cache
- `C-u C-u u` clear the cache and refresh.
- `SPC` Collapse/Expand node at point.
- `C-u SPC` Collapse/Expand node at point and its children.

## M-x customize

If the extension has been loaded, simply call `M-x customize-group` and type `mu4e-maildirs-extension`.

### mu4e-maildirs-extension-action-key

Key shortcut to update index and cache.

### mu4e-maildirs-extension-toggle-node-key

Key shortcut to expand/collapse node at point.

### mu4e-maildirs-extension-action-text

Action text to display for updating the index and cache.
If set to 'Don't Display (nil)' it won't be displayed.

### mu4e-maildirs-extension-count-command-format

The command to count a maildir.  [Most people won't need to edit this].

### mu4e-maildirs-extension-custom-list

Custom list of folders to show.

### mu4e-maildirs-extension-insert-before-str

The place where the maildirs section should be inserted. The valid options are `Basics`, `Bookmarks`, `Misc` and `End of file`.

### mu4e-maildirs-extension-node-format

The node format.

### mu4e-maildirs-extension-before-insert-node-hook

Hook called before inserting a node.

### mu4e-maildirs-extension-after-insert-node-hook

Hook called after inserting a node.

### mu4e-maildirs-extension-propertize-func

The function to format the maildir info.
Default dispays as '| maildir_name (unread/total)'.

### mu4e-maildirs-extension-node-indent

Node indentation.

### mu4e-maildirs-extension-node-indent-char

The char used for indentation.

### mu4e-maildirs-extension-default-collapse-level

The default level to collapse nodes.
Set `nil' to disable.

### mu4e-maildirs-extension-node-collapsed-prefix

The prefix for collapsed node.

### mu4e-maildirs-extension-node-expanded-prefix

The prefix for expanded node.

### mu4e-maildirs-extension-node-default-prefix

The prefix for default node.

### mu4e-maildirs-extension-fake-maildir-separator

The separator to fake a hierarchy using directory names.
For example:
/Archive
/Archive.foo
/Archive.foo.bar
/Archive.baz

Offlineimap does this when setting `sep = .'.

### mu4e-maildirs-extension-node-face

Face for a normal node.

### mu4e-maildirs-extension-node-unread-face

Face for a node containing unread items.

### mu4e-maildirs-extension-parallel-processes

Max parallel processes.


## Write your own node format handler

If you need more customization you can change the default format `| maildir_name (unread/total)` providing your own function. For example, to highlight only the unread count you could use something like this in your `.emacs`:

```lisp
(defun my/mu4e-maildirs-extension-propertize-unread-only (item)
  "Propertize only the maildir unread count using ITEM plist."
  (format "\t%s%s %s (%s/%s)"
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

## Update index outside emacs

If you update the index outside emacs (by calling `mu` directly) you will need to update the `mu4e-main-view` using `C-u C-u u` or calling `(mu4e-maildirs-extension-force-update '(16))`


## Changelog

Short summary of changes:

v0.9 (breaking changes):
    - Update variables to make customizations easier.
    - Allow nodes at point to collapse/expand.
    - Add async support

v0.8:
    - Auto-update `mu4e-main-view` if the index have changed and the buffer is visible.
    - Use universal argument to be able to manually clear the cache and refresh (`C-u u` and `C-u C-u u`)
