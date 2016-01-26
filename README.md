[![MELPA Stable](http://stable.melpa.org/packages/mu4e-maildirs-extension-badge.svg)](http://stable.melpa.org/#/mu4e-maildirs-extension)
[![MELPA](http://melpa.org/packages/mu4e-maildirs-extension-badge.svg)](http://melpa.org/#/mu4e-maildirs-extension)
[![Build Status](https://travis-ci.org/agpchil/mu4e-maildirs-extension.svg)](https://travis-ci.org/agpchil/mu4e-maildirs-extension)

**master** branch is **experimental**

# Mu4e maildirs extension

This extension adds a maildir summary in `mu4e-main-view`.

It runs a `mu` command (async) in a shell process for each maildir to count unread and total mails.


![Screenshot](https://drive.google.com/uc?export=view&id=0Byv-S6nIE7oRVm85UGVxY3FqMUE)

## Requirements
This extension needs [mu4e](http://github.com/djcb/mu) version 0.9.9.5 or newer to work.

## Directory structure

This extension expects the following maildir structure in `mu4e-maildir` directory

```
account1/
  inbox/
  drafts/
  ...
account2/
  inbox/
  drafts/
  ...
```

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
- `SPC` Collapse/Expand maildir at point.
- `C-u SPC` Collapse/Expand maildir at point and its children.

## M-x customize

If the extension has been loaded, simply call `M-x customize-group` and type `mu4e-maildirs-extension`.

### mu4e-maildirs-extension-action-key

Key shortcut to update index and cache.

### mu4e-maildirs-extension-action-text

Action text to display for updating the index and cache.
If set to 'Don't Display (nil)' it won't be displayed.

### mu4e-maildirs-extension-after-insert-maildir-hook

Hook called after inserting a maildir.

### mu4e-maildirs-extension-before-insert-maildir-hook

Hook called before inserting a maildir.

### mu4e-maildirs-extension-bookmark-format

The bookmark stats format.

Available formatters:

%u is the unread count
%t is the total count

### mu4e-maildirs-extension-bookmark-format-spec

A function to build the bookmark format spec.

### mu4e-maildirs-extension-bookmark-hl-pred

Predicate function used to highlight.

### mu4e-maildirs-extension-bookmark-hl-regex

Regex to highlight when `mu4e-maildirs-extension-bookmark-hl-pred' matches.

### mu4e-maildirs-extension-count-command-format

The command to count a maildir.  [Most people won't need to edit this].

### mu4e-maildirs-extension-custom-list

List of folders to show.
If set to nil all folders are shown.

Example:
  '("/account1/INBOX"
    "/account2/INBOX")

### mu4e-maildirs-extension-default-collapse-level

The default level to collapse maildirs.
Set `nil' to disable.

### mu4e-maildirs-extension-fake-maildir-separator

The separator to fake a hierarchy using directory names.
For example:
```
/Archive
/Archive.foo
/Archive.foo.bar
/Archive.baz
```

Offlineimap does this when setting `sep = .'.

### mu4e-maildirs-extension-insert-before-str

The place where the maildirs section should be inserted.

### mu4e-maildirs-extension-maildir-collapsed-prefix

The prefix for collapsed maildir.

### mu4e-maildirs-extension-maildir-default-prefix

The prefix for default maildir.

### mu4e-maildirs-extension-maildir-expanded-prefix

The prefix for expanded maildir.

### mu4e-maildirs-extension-maildir-format

The maildir format.

Available formatters:

%i is the folder indentation
%p is the maildir prefix
%l is the folder level
%e is the expand flag
%P is the maildir path
%n is the maildir name
%u is the unread count
%t is the total count

### mu4e-maildirs-extension-maildir-format-spec

A function to build the maildir format spec.

### mu4e-maildirs-extension-maildir-hl-pred

Predicate function used to highlight.

### mu4e-maildirs-extension-maildir-hl-regex

Regex to highlight when `mu4e-maildirs-extension-maildir-hl-pred' matches.

### mu4e-maildirs-extension-maildir-indent

Maildir indentation.

### mu4e-maildirs-extension-maildir-indent-char

The char used for indentation.

### mu4e-maildirs-extension-parallel-processes

Max parallel processes.

### mu4e-maildirs-extension-propertize-bm-func

The function to format the bookmark info.
Default dispays as ' (unread/total)'.

### mu4e-maildirs-extension-propertize-func

The function to format the maildir info.
Default dispays as '| maildir_name (unread/total)'.

### mu4e-maildirs-extension-title

The title for the maildirs extension section.
If set to `nil' it won't be displayed.

### mu4e-maildirs-extension-toggle-maildir-key

Key shortcut to expand/collapse maildir at point.

### mu4e-maildirs-extension-updating-string

The string to show while updating in background.

### mu4e-maildirs-extension-use-bookmarks

If non-nil, show the bookmarks count in the mu4e main view.

### mu4e-maildirs-extension-use-maildirs

If non-nil, show the maildir summary in the mu4e main view.

### mu4e-maildirs-extension-maildir-face

Face for a normal maildir.

### mu4e-maildirs-extension-maildir-hl-face

Face for a highlighted maildir.

## Write your own maildir format handler

If you need more customization you can change the default format `| maildir_name (unread/total)` providing your own function. For example, to highlight only the unread count you could use something like this in your `.emacs`:

```lisp
(defun my/mu4e-maildirs-extension-propertize-unread-only (item)
  "Propertize only the maildir unread count using ITEM plist."
  (let ((unread (or (plist-get item :unread) 0))
        (total (or (plist-get item :total) 0)))
    (format "\t%s%s %s (%s/%s)"
            (plist-get item :indent)
            (plist-get item :prefix)
            (plist-get item :name)
            (propertize (number-to-string unread)
                        'face (cond
                               ((> unread 0) 'mu4e-maildirs-extension-maildir-hl-face)
                               (t            'mu4e-maildirs-extension-maildir-face)))
            total)))
```

Then set `mu4e-maildirs-extension-propertize-func` to `my/mu4e-maildirs-extension-propertize-unread-only` in the `customize-group` area.

## Update index outside emacs

If you update the index outside emacs (by calling `mu` directly) you will need to update the `mu4e-main-view` using `C-u C-u u` or calling `(mu4e-maildirs-extension-force-update '(16))`


## Changelog

Short summary of changes:

* master:
  - Add variables:
    - mu4e-maildirs-extension-use-bookmarks
    - mu4e-maildirs-extension-use-maildirs
  - Add bookmarks support (experimental)
  - Improve customizations.
  - Add new highlight options.
  - Add -load/-unload functions
  - Rename variables (old -> new):
    - mu4e-maildirs-extension-submaildir-indent -> mu4e-maildirs-extension-maildir-indent
    - mu4e-maildirs-extension-maildir-separator -> mu4e-maildirs-extension-maildir-collapsed-prefix
    - mu4e-maildirs-extension-submaildir-separator -> mu4e-maildirs-extension-maildir-default-prefix
    - mu4e-maildirs-extension-maildir-unread-face -> mu4e-maildirs-extension-maildir-hl-face
    - mu4e-maildirs-extension-cached-maildirs-data -> mu4e-maildirs-extension-maildirs
  - Add new variables:
    - mu4e-maildirs-extension-maildir-format
    - mu4e-maildirs-extension-maildir-format-spec
    - mu4e-maildirs-extension-maildir-hl-regex
    - mu4e-maildirs-extension-maildir-hl-pred
    - mu4e-maildirs-extension-before-insert-maildir-hook
    - mu4e-maildirs-extension-after-insert-maildir-hook
    - mu4e-maildirs-extension-maildir-indent-char
    - mu4e-maildirs-extension-default-collapse-level
    - mu4e-maildirs-extension-maildir-expanded-prefix
    - mu4e-maildirs-extension-fake-maildir-separator
    - mu4e-maildirs-extension-parallel-processes
  - Allow maildirs at point to collapse/expand.
  - Add async support

* v0.8:
  - Auto-update `mu4e-main-view` if the index have changed and the buffer is visible.
  - Use universal argument to be able to manually clear the cache and refresh (`C-u u` and `C-u C-u u`)
