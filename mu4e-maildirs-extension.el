;;; mu4e-maildirs-extension.el --- Show mu4e maildirs summary in mu4e-main-view

;; This file is not part of Emacs

;; Copyright (C) 2013 Andreu Gil Pàmies

;; Filename: mu4e-maildirs-extension.el
;; Version: 0.1
;; Author: Andreu Gil Pàmies <agpchil@gmail.com>
;; Created: 22-07-2013
;; Description: Show mu4e maildirs summary in mu4e-main-view with unread and
;; total mails for each maildir
;; URL: http://github.com/agpchil/mu4e-maildirs-extension

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;; (require 'mu4e-maildirs-extension)
;; (mu4e-maildirs-extension)

;;; Commentary:

;;; Code:
(require 'mu4e)
(require 'dash)

(defgroup mu4e-maildirs-extension nil
  "Show mu4e maildirs summary in mu4e-main-view with unread and
total mails for each maildir."
  :link '(url-link "https://github.com/agpchil/mu4e-maildirs-extension")
  :prefix "mu4e-maildirs-extension-"
  :group 'external)

(defcustom mu4e-maildirs-extension-action-key "u"
  "Key shortcut to update index and cache."
  :group 'mu4e-maildirs-extension
  :type '(key-sequence))

(defcustom mu4e-maildirs-extension-action-text "\t* [u]pdate index & cache\n"
  "Action text to display for updating the index and cache.
If set to 'Don't Display (nil)' it won't be displayed."
  :group 'mu4e-maildirs-extension
  :type '(choice string (const :tag "Don't Display" nil)))

(defcustom mu4e-maildirs-extension-count-command-format
  "mu find %s maildir:'%s' --fields 'i' 2>/dev/null |wc -l |tr -d '\n'"
  "The command to count a maildir.  [Most people won't need to edit this]."
  :group 'mu4e-maildirs-extension
  :type '(string))

(defcustom mu4e-maildirs-extension-custom-list nil
  "Custom list of folders to show."
  :group 'mu4e-maildirs-extension
  :type '(repeat string))
  ;; :type '(sexp))

(defcustom mu4e-maildirs-extension-insert-before-str "\n  Misc"
  "The place where the maildirs summary should be inserted."
  :group 'mu4e-maildirs-extension
  :type '(choice (const :tag "Basics" "\n  Basics")
                 (const :tag "Bookmarks" "\n  Bookmarks")
                 (const :tag "Misc" "\n  Misc")))

(defcustom mu4e-maildirs-extension-maildir-separator "+"
  "The separator for each top level mail directory."
  :group 'mu4e-maildirs-extension
  :type '(string))

(defcustom mu4e-maildirs-extension-before-insert-maildir-hook
  '(mu4e-maildirs-extension-insert-newline-if-top-maildir)
  "Hook called before inserting a maildir."
  :group 'mu4e-maildirs-extension
  :type 'hook)

(defcustom mu4e-maildirs-extension-after-insert-maildir-hook
  '(mu4e-maildirs-extension-insert-newline)
  "Hook called after inserting a maildir."
  :group 'mu4e-maildirs-extension
  :type 'hook)

(defcustom mu4e-maildirs-extension-propertize-func
  #'mu4e-maildirs-extension-propertize-handler
  "The function to format the maildir info.
Default dispays as '| maildir_name (unread/total)'."
  :group 'mu4e-maildirs-extension
  :type '(function))

(defcustom mu4e-maildirs-extension-submaildir-indent 2
  "Indentation of submaildirs."
  :group 'mu4e-maildirs-extension
  :type '(integer))

(defcustom mu4e-maildirs-extension-submaildir-separator "|"
  "The separator for each sub-level mail directory."
  :group 'mu4e-maildirs-extension
  :type '(string))

(defcustom mu4e-maildirs-extension-fake-maildir-separator nil
  "The separator to fake a hierarchy using directory names.
For example:
/Archive
/Archive.foo
/Archive.foo.bar
/Archive.baz

Offlineimap does this when setting `sep = .'."
  :group 'mu4e-maildirs-extension
  :type '(string))

(defcustom mu4e-maildirs-extension-title "  Maildirs\n"
  "The title label for the maildirs extension."
  :group 'mu4e-maildirs-extension
  :type '(choice string (const :tag "Don't Display" nil)))

(defface mu4e-maildirs-extension-maildir-face
  '((t :inherit mu4e-header-face))
  "Face for a normal maildir."
  :group 'mu4e-maildirs-extension)

(defface mu4e-maildirs-extension-maildir-unread-face
  '((t :inherit mu4e-unread-face))
  "Face for a maildir containing unread items."
  :group 'mu4e-maildirs-extension)

(defvar mu4e-maildirs-extension-start-point nil)

(defvar mu4e-maildirs-extension-end-point nil)

(defvar mu4e-maildirs-extension-maildirs nil)

(defvar mu4e-maildirs-extension-parallel-processes 6)
(defvar mu4e-maildirs-extension-running-processes 0)
(defvar mu4e-maildirs-extension-queue nil)

(defvar mu4e-maildirs-extension-buffer-name mu4e~main-buffer-name)

(defvar mu4e-maildirs-extension-index-updated-func
  'mu4e-maildirs-extension-index-updated-handler)

(defvar mu4e-maildirs-extension-main-view-func
  'mu4e-maildirs-extension-main-view-handler)

(defun mu4e-maildirs-extension-index-updated-handler ()
  "Handler for `mu4e-index-updated-hook'."
  (let ((arg (if (get-buffer-window mu4e-maildirs-extension-buffer-name)
                 '(16)
               '(4))))
    (mu4e-maildirs-extension-force-update arg)))

(defun mu4e-maildirs-extension-main-view-handler ()
  "Handler for `mu4e-main-view-mode-hook'."
  (setq mu4e-maildirs-extension-start-point nil)
  (mu4e-maildirs-extension-update))

(defmacro mu4e-maildirs-extension-with-buffer (&rest body)
  "Switch to `mu4e-maildirs-extension' buffer and yield BODY."
  (declare (indent 1))
  `(let* ((buffer (get-buffer mu4e-maildirs-extension-buffer-name))
          (buffer-window (car (get-buffer-window-list buffer)))
          (inhibit-read-only t))
     (when buffer
       (cond (buffer-window
              (with-selected-window buffer-window
                (save-excursion
                  ,@body)))
             (t
              (with-current-buffer buffer
                (save-excursion
                  ,@body)))))))

(defun mu4e-maildirs-extension-unqueue-maybe ()
  (when (< mu4e-maildirs-extension-running-processes
           mu4e-maildirs-extension-parallel-processes)
    (let ((proc-func (pop mu4e-maildirs-extension-queue)))
      (when proc-func
        (funcall proc-func)
        (setq mu4e-maildirs-extension-running-processes
              (1+ mu4e-maildirs-extension-running-processes))))))

(defun mu4e-maildirs-extension-fetch (mdir opts &optional callback)
  "Fetch the result of executing the command for a MDIR with optional OPTS."
  (let* ((cmd (format mu4e-maildirs-extension-count-command-format
                      opts
                      mdir))
         (finish-func `(lambda(proc event)
                         (when (and (equal event "finished\n")
                                    (buffer-live-p (process-buffer proc))
                                    ,callback)
                           ;; (memq (process-status proc) '(exit))
                           (let ((buffer (process-buffer proc))
                                 (result nil))
                             (with-current-buffer buffer
                               (setq result (string-to-number
                                             (replace-regexp-in-string "![0-9]"
                                                                       ""
                                                                       (buffer-string)))))
                             (mu4e-maildirs-extension-with-buffer
                                 (funcall ,callback result))
                             (kill-buffer buffer)
                             (setq mu4e-maildirs-extension-running-processes
                                   (1- mu4e-maildirs-extension-running-processes))
                             (mu4e-maildirs-extension-unqueue-maybe)))))
         (proc `(lambda()
                  (let ((proc (start-process-shell-command "mu4e-maildirs-extension"
                                                           (make-temp-name "mu4e-maildirs-extension")
                                                           ,cmd)))
                    (set-process-sentinel proc ,finish-func)))))

    (add-to-list 'mu4e-maildirs-extension-queue proc t)
    (mu4e-maildirs-extension-unqueue-maybe)))

(defun mu4e-maildirs-extension-parse-maildirs (path)
  "Get the maildir parents of maildir PATH name.
Given PATH \"/foo/bar/alpha\" will return '(\"/foo\" \"/bar\")."
  (let ((name (replace-regexp-in-string "^/" "" path))
        (parents nil)
        (fake-sep mu4e-maildirs-extension-fake-maildir-separator)
        (all-parents nil))
    (setq name (replace-regexp-in-string "\\/\\*$" "" name))
    (setq parents (split-string name "/" t))
    (cond (mu4e-maildirs-extension-fake-maildir-separator
           (mapc #'(lambda(s)
                     (setq all-parents (append all-parents (split-string s fake-sep t))))
                 parents))
          (t (setq all-parents parents)))
    all-parents))

(defun mu4e-maildirs-extension-get-maildirs ()
  "Get maildirs."
  (let ((maildirs (or mu4e-maildirs-extension-custom-list
                      (mu4e-get-maildirs)))
        (maildirs-to-show nil))

    (mapc #'(lambda (name)
              (let ((parents (butlast (mu4e-maildirs-extension-parse-maildirs name)))
                    (path nil))
                 (mapc #'(lambda (parent-name)
                            (setq path (concat path "/" parent-name))
                            (unless (member path maildirs-to-show)
                              (add-to-list 'maildirs-to-show (format "%s/*" path) t)))
                         parents))

               (add-to-list 'maildirs-to-show name t))
            maildirs)
    maildirs-to-show))

(defun mu4e-maildirs-extension-is-parent-p (name maildirs)
  "Check if NAME is a parent maildir from MAILDIRS."
  (when (string-match "\\/\\*$" name)
    (--first (and (not (equal name it))
                  (string-match name it))
             maildirs)))

(defun mu4e-maildirs-extension-new-maildir (path is-parent-p)
  "Build new maildir plist from maildir PATH and IS-PARENT-P flag."
  (let* ((item nil)
         (current-maildirs (mu4e-maildirs-extension-parse-maildirs path))
         (level (1- (length current-maildirs))))
    (setq item (plist-put item
                          :name (car (last current-maildirs))))
    (setq item (plist-put item
                          :level
                          level))
    (setq item (plist-put item
                          :path
                          path))
    (setq item (plist-put item
                          :separator
                          (if (or is-parent-p (equal level 0))
                              mu4e-maildirs-extension-maildir-separator
                            mu4e-maildirs-extension-submaildir-separator)))
    (setq item (plist-put item
                          :indent
                          (make-string (* mu4e-maildirs-extension-submaildir-indent level)
                                       32)))
    (setq item (plist-put item
                          :total
                          nil))
    (setq item (plist-put item
                          :unread
                          nil))
    item))

(defun mu4e-maildirs-extension-propertize-handler (item)
  "Propertize the maildir text using ITEM plist."
  (propertize (format "\t%s%s %s (%s/%s)"
                      (plist-get item :indent)
                      (plist-get item :separator)
                      (plist-get item :name)
                      (or (plist-get item :unread) "?")
                      (or (plist-get item :total) "?"))
              'face (cond
                     ((> (or (plist-get item :unread) 0) 0) 'mu4e-maildirs-extension-maildir-unread-face)
                     (t            'mu4e-maildirs-extension-maildir-face))))

(defun mu4e-maildirs-extension-load-maildirs ()
  "Fetch data if no cache."
  (unless mu4e-maildirs-extension-maildirs
    (let ((maildirs (mu4e-maildirs-extension-get-maildirs)))
      (setq mu4e-maildirs-extension-maildirs
           (mapcar #'(lambda(m)
                       (let ((is-parent-p (mu4e-maildirs-extension-is-parent-p m maildirs)))
                         (mu4e-maildirs-extension-new-maildir m is-parent-p)))
                   maildirs))))
  mu4e-maildirs-extension-maildirs)

(defun mu4e-maildirs-extension-action-str (str &optional func-or-shortcut)
  "Custom action without using [.] in STR.
If FUNC-OR-SHORTCUT is non-nil and if it is a function, call it
when STR is clicked (using RET or mouse-2); if FUNC-OR-SHORTCUT is
a string, execute the corresponding keyboard action when it is
clicked."
  (let ((newstr str)
        (map (make-sparse-keymap))
        (func (if (functionp func-or-shortcut)
                  func-or-shortcut
                (if (stringp func-or-shortcut)
                    (lexical-let ((macro func-or-shortcut))
                      (lambda()(interactive)
                        (execute-kbd-macro macro)))))))
    (define-key map [mouse-2] func)
    (define-key map (kbd "RET") func)
    (put-text-property 0 (length newstr) 'keymap map newstr)
    (put-text-property (string-match "[^\n\t\s-].+$" newstr)
      (- (length newstr) 1) 'mouse-face 'highlight newstr)
    newstr))

(defun mu4e-maildirs-extension-insert-newline-if-top-maildir (m)
  "Insert a newline if M is a top level maildir."
  (when (equal (plist-get m :level) 0)
    (insert "\n")))

(defun mu4e-maildirs-extension-insert-newline (m)
  "Insert a newline."
  (insert "\n"))

(defun mu4e-maildirs-extension-insert-maildir (m marker)
  "Insert maildir entry into mu4e main view."
  (goto-char marker)
  (delete-region (point-at-bol) (point-at-eol))
  (beginning-of-line)
  (insert (mu4e-maildirs-extension-action-str
           (funcall mu4e-maildirs-extension-propertize-func m)
           `(lambda ()
              (interactive)
              (mu4e~headers-jump-to-maildir ,(plist-get m :path))))))


(defun mu4e-maildirs-extension-update ()
  "Insert maildirs summary in `mu4e-main-view'."

  (let ((maildirs (mu4e-maildirs-extension-load-maildirs)))
    (mu4e-maildirs-extension-with-buffer
        (goto-char (point-max))
      (if (and mu4e-maildirs-extension-start-point
               mu4e-maildirs-extension-end-point)
          (delete-region mu4e-maildirs-extension-start-point
                         mu4e-maildirs-extension-end-point)
        (setq mu4e-maildirs-extension-start-point
              (search-backward mu4e-maildirs-extension-insert-before-str)))

      ;; persistent end-point mark
      (setq mu4e-maildirs-extension-end-point (make-marker))
      (set-marker mu4e-maildirs-extension-end-point mu4e-maildirs-extension-start-point)
      (set-marker-insertion-type mu4e-maildirs-extension-end-point t)

      (goto-char mu4e-maildirs-extension-start-point)

      (when mu4e-maildirs-extension-title
        (insert "\n"
                (propertize mu4e-maildirs-extension-title 'face 'mu4e-title-face)))

      (when mu4e-maildirs-extension-action-text
        (insert "\n"
                (mu4e~main-action-str mu4e-maildirs-extension-action-text
                                      mu4e-maildirs-extension-action-key)))

      (define-key mu4e-main-mode-map
        mu4e-maildirs-extension-action-key
        'mu4e-maildirs-extension-force-update)

      (mapc #'(lambda (item)
                (run-hook-with-args 'mu4e-maildirs-extension-before-insert-maildir-hook
                                    item)
                (let* ((path (plist-get item :path))
                       (marker (point-marker))
                       (insert-cb `(lambda(m)
                                     (mu4e-maildirs-extension-insert-maildir m  ,marker)))
                       (unread-cb `(lambda(result)
                                     (let ((m (--first (equal (plist-get it :path) ,path)
                                                          mu4e-maildirs-extension-maildirs)))
                                       (setq m (plist-put m :unread result))
                                       (funcall ,insert-cb m))))
                       (total-cb `(lambda(result)
                                    (let ((m (--first (equal (plist-get it :path) ,path)
                                                         mu4e-maildirs-extension-maildirs)))
                                      (setq m (plist-put m :total result))
                                      (funcall ,insert-cb m)
                                      ))))
                  (funcall insert-cb item)
                  (run-hook-with-args 'mu4e-maildirs-extension-after-insert-maildir-hook
                                      item)
                  (unless (plist-get item :unread)
                    (mu4e-maildirs-extension-fetch path "flag:unread" unread-cb))
                  (unless (plist-get item :total)
                    (mu4e-maildirs-extension-fetch path "" total-cb))))
            maildirs))))

(defun mu4e-maildirs-extension-force-update (&optional universal-arg)
  "Force update cache and summary.
Default behaviour calls `mu4e-update-index' and update cache/summary if needed.
When preceded with `universal-argument':
4 = clears the cache,
16 = clears the cache and update the summary."
  (interactive "P")
  (cond ((equal universal-arg nil)
         (mu4e-update-index))
        ((equal universal-arg '(4))
         (setq mu4e-maildirs-extension-maildirs nil))
        ((equal universal-arg '(16))
         (setq mu4e-maildirs-extension-maildirs nil)
         (mu4e-maildirs-extension-update))))

;;;###autoload
(defun mu4e-maildirs-extension ()
  "Initialize."
  (remove-hook 'mu4e-index-updated-hook mu4e-maildirs-extension-index-updated-func)
  (add-hook 'mu4e-index-updated-hook mu4e-maildirs-extension-index-updated-func)

  (remove-hook 'mu4e-main-mode-hook mu4e-maildirs-extension-main-view-func)
  (add-hook 'mu4e-main-mode-hook mu4e-maildirs-extension-main-view-func))

(provide 'mu4e-maildirs-extension)
;;; mu4e-maildirs-extension.el ends here
