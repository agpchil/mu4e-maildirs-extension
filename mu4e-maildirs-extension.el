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

(defvar mu4e-maildirs-extension-start-point nil)
(defvar mu4e-maildirs-extension-end-point nil)
(defvar mu4e-maildirs-extension-title "  Maildirs\n")
(defvar mu4e-maildirs-extension-undefined-maildir-name "default account")
(defvar mu4e-maildirs-extension-maildir-separator "\n\t» ")
(defvar mu4e-maildirs-extension-submaildir-separator "\t  | ")
(defvar mu4e-maildirs-extension-insert-before-str "\n  Misc")
(defvar mu4e-maildirs-extension-cached-maildirs-count nil)
(defvar mu4e-maildirs-extension-buffer-name mu4e~main-buffer-name)
(defvar mu4e-maildirs-extension-count-command-format
  "mu find %s maildir:'%s' --fields 'i' 2>/dev/null |wc -l |tr -d '\n'")
(defvar mu4e-maildirs-extension-index-updated-func
  'mu4e-maildirs-extension-index-updated-handler)
(defvar mu4e-maildirs-extension-main-view-func
  'mu4e-maildirs-extension-main-view-handler)

(defun mu4e-maildirs-extension-index-updated-handler ()
  "Handler for mu4e-index-updated-hook."
  (setq mu4e-maildirs-extension-cached-maildirs-count nil)
  (when (equal (buffer-name) mu4e-maildirs-extension-buffer-name)
    (mu4e-maildirs-extension-insert-summary)))

(defun mu4e-maildirs-extension-main-view-handler ()
  "Handler for mu4e-main-view-mode-hook."
  (setq mu4e-maildirs-extension-start-point nil)
  (mu4e-maildirs-extension-insert-summary))

(defun mu4e-maildirs-extension-get-parent-name (mdir)
  "Get the parent maildir name from a MDIR path."
  (when (and mdir (string-match "^/\\(.*?\\)/" mdir))
    (match-string 1 mdir)))

(defun mu4e-maildirs-extension-execute-count (mdir &optional opts)
  "Execute the count command for a MDIR with optional OPTS."
  (let* ((mu-opts (if opts opts ""))
         (cmd (format mu4e-maildirs-extension-count-command-format
                      mu-opts
                      mdir)))
    (string-to-number (replace-regexp-in-string "![0-9]"
                                                ""
                                                (shell-command-to-string cmd)))))

(defun mu4e-maildirs-extension-count-mails ()
  "Count mails in maildirs."
  (let ((maildirs (mu4e-get-maildirs))
        (mdir nil)
        (total 0)
        (unread 0)
        (result nil))
    (dolist (mdir maildirs)
      (setq total (mu4e-maildirs-extension-execute-count mdir))
      (setq unread (mu4e-maildirs-extension-execute-count mdir "flag:unread"))
      (setq result (cons `(,mdir (,unread ,total)) result)))
    (reverse result)))

(defun mu4e-maildirs-extension-propertize (item)
  "Propertize ITEM.
ITEM is an alist with the following structure
'(maildir_name (unread_count total_count))."
  (let ((name (car item))
        (unread (car (cadr item)))
        (total (cadr (cadr item))))

    (setq name (if (mu4e-maildirs-extension-get-parent-name name)
                   (concat mu4e-maildirs-extension-submaildir-separator
                           (replace-regexp-in-string "^/[^/]*" "" name))
                 (concat mu4e-maildirs-extension-submaildir-separator name)))

    (setq unread (if (> unread 0)
                     (propertize (number-to-string unread) 'face 'mu4e-view-header-key-face)
                   (number-to-string unread)))

    (format "%s (%s/%s)\n" name unread total)))

(defun mu4e-maildirs-extension-insert-item (item prev)
  "Insert ITEM.
ITEM is an alist with the following structure
'(maildir_name (unread_count total_count)).

Insert the parent maildir name if ITEM has a different one from PREV."
  (let ((parent-name (mu4e-maildirs-extension-get-parent-name (car item)))
        (prev-parent-name (mu4e-maildirs-extension-get-parent-name (car prev))))

    (unless parent-name
      (setq parent-name mu4e-maildirs-extension-undefined-maildir-name)
      (setf (car item) (concat "/" parent-name (car item))))

    (when (not (equal prev-parent-name parent-name))
     (insert (concat mu4e-maildirs-extension-maildir-separator parent-name "\n")))

    (insert
     (mu4e~main-action-str (mu4e-maildirs-extension-propertize item)
                           `(lambda ()
                              (interactive)
                              (mu4e~headers-jump-to-maildir ,(car item)))))))

(defun mu4e-maildirs-extension-insert-summary ()
  "Insert maildirs summary in mu4e-main-view."
  (unless mu4e-maildirs-extension-cached-maildirs-count
    (setq mu4e-maildirs-extension-cached-maildirs-count
          (mu4e-maildirs-extension-count-mails)))

  (let ((buf (get-buffer mu4e-maildirs-extension-buffer-name))
        (maildirs mu4e-maildirs-extension-cached-maildirs-count)
        (item nil)
        (prev nil)
        (inhibit-read-only t))
    (when buf
      (with-current-buffer buf
        (if mu4e-maildirs-extension-start-point
            (delete-region mu4e-maildirs-extension-start-point
                           mu4e-maildirs-extension-end-point)
          (setq mu4e-maildirs-extension-start-point
                (search-backward mu4e-maildirs-extension-insert-before-str)))

        (goto-char mu4e-maildirs-extension-start-point)

        (insert "\n"
                (propertize mu4e-maildirs-extension-title 'face 'mu4e-title-face))

        (insert "\n"
                (mu4e~main-action-str "\t* [u]pdate index & cache\n" "u"))
        (define-key mu4e-main-mode-map "u" 'mu4e-maildirs-extension-insert-summary-no-cache)

        (dolist (item maildirs)
          (mu4e-maildirs-extension-insert-item item prev)
          (setq prev item))

        (setq mu4e-maildirs-extension-end-point (point))
        (goto-char (point-min))))))

(defun mu4e-maildirs-extension-insert-summary-no-cache ()
  "Clear cache and insert maildirs summary."
  (interactive)
  (mu4e-message "Updating index & cache...")
  (mu4e-update-index))

;;;###autoload
(defun mu4e-maildirs-extension ()
  "Initialize."
  (remove-hook 'mu4e-index-updated-hook mu4e-maildirs-extension-index-updated-func)
  (add-hook 'mu4e-index-updated-hook mu4e-maildirs-extension-index-updated-func)

  (remove-hook 'mu4e-main-mode-hook mu4e-maildirs-extension-main-view-func)
  (add-hook 'mu4e-main-mode-hook mu4e-maildirs-extension-main-view-func))

(provide 'mu4e-maildirs-extension)
;;; mu4e-maildirs-extension.el ends here
