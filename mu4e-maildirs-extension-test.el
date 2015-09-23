;;; mu4e-maildirs-extension-test.el --- mu4e-maildirs-extension tests

;; This file is not part of Emacs

;; Copyright (C) 2015 Andreu Gil Pàmies

;; Filename: mu4e-maildirs-extension-test.el
;; Version: 0.1
;; Author: Andreu Gil Pàmies <agpchil@gmail.com>
;; Created: 20-09-2015
;; Description: mu4e-maildirs-extension tests
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

;;; Commentary:

;;; Code:
(require 'mu4e-maildirs-extension)

(ert-deftest mu4e-maildirs-extension-parse/with-normal-path ()
  "Tests the parsing of a normal path."
  (let* ((path "/account1/maildir1/maildir2")
         (items (mu4e-maildirs-extension-parse path)))
    (should (equal items '("account1" "maildir1" "maildir2")))))

(ert-deftest mu4e-maildirs-extension-parse/with-fake-path ()
  "Tests the parsing of a path with fake separators."
  (let* ((path "/account1.maildir1.maildir2")
         (mu4e-maildirs-extension-fake-maildir-separator "\\.")
         (items (mu4e-maildirs-extension-parse path)))
    (should (equal items '("account1" "maildir1" "maildir2")))))

(ert-deftest mu4e-maildirs-extension-parse/with-fake-path-wrong-separator ()
  "Tests the parsing of a path using wrong fake separators."
  (let* ((path "/account1.maildir1.maildir2")
         (mu4e-maildirs-extension-fake-maildir-separator "\\#")
         (items (mu4e-maildirs-extension-parse path)))
    (should (equal items '("account1.maildir1.maildir2")))))


(ert-deftest mu4e-maildirs-extension-paths/with-submaildirs-maildirs ()
  "Tests fetching the list of maildirs paths.
Each subdirectory is a valid submaildir too.
A valid maildir has `cur', `new' and `tmp' directories"
  (let* ((mu4e-maildirs-extension-custom-list '("/account1/inbox"
                                                "/account1/archive" ;; valid maildir
                                                "/account1/archive/old" ;; valid maildir
                                                "/account1/archive/old/older"
                                                "/account1/sent"
                                                "/account1/trash"))
         (items (mu4e-maildirs-extension-paths)))
    (should (equal items '("/account1/*"
                           "/account1/inbox"
                           "/account1/archive"
                           "/account1/archive/old"
                           "/account1/archive/old/older"
                           "/account1/sent"
                           "/account1/trash")))))

(ert-deftest mu4e-maildirs-extension-paths/with-submaildirs-dirs ()
  "Tests fetching the list of maildirs paths.
Each subdirectory is not a valid submaildir but a dir containing submaildirs."
  (let* ((mu4e-maildirs-extension-custom-list '("/account1/inbox"
                                                ;; this dirs don't exist!
                                                ;; /account1/archive/{cur,new,tmp}
                                                "/account1/archive/old" ;; valid maildir
                                                "/account1/sent"
                                                "/account1/trash"))
         (items (mu4e-maildirs-extension-paths)))
    (should (equal items '("/account1/*"
                           "/account1/inbox"
                           "/account1/archive/*" ;; <--- virtual submaildirs query
                           "/account1/archive/old"
                           "/account1/sent"
                           "/account1/trash")))))


(ert-deftest mu4e-maildirs-extension-new-maildir ()
  "Tests the maildir plist creation."
  (let* ((path "/account1/inbox")
         (m (mu4e-maildirs-extension-new-maildir path)))
    (should (equal m '(:name "inbox"
                       :level 1
                       :expand t
                       :path "/account1/inbox"
                       :indent "  "
                       :total nil
                       :unread nil)))))

(ert-deftest mu4e-maildirs-extension-roots ()
  "Tests fetching the roots of a maildir."
  (let* ((path-list '("/account1/*" "/account1/inbox/personal" "/account1/inbox/work"))
         (l (mapcar (lambda(path)
                      (mu4e-maildirs-extension-new-maildir path))
                    path-list))
         (roots (mu4e-maildirs-extension-roots l)))
    (should (equal roots '((:name "account1"
                            :level 0
                            :expand t
                            :path "/account1/*"
                            :indent ""
                            :total nil
                            :unread nil))))))

(ert-deftest mu4e-maildirs-extension-children/all-from-root ()
  "Tests fetching the children from root."
  (let* ((path-list '("/account1/*" "/account1/inbox/*"
                      "/account1/inbox/personal" "/account1/inbox/work"))
         (l (mapcar (lambda(path)
                      (mu4e-maildirs-extension-new-maildir path))
                    path-list))
         (m (mu4e-maildirs-extension-new-maildir "/account1/*"))
         (children (mu4e-maildirs-extension-children m l)))
    (should (equal children '((:name "inbox"
                               :level 1
                               :expand t
                               :path "/account1/inbox/*"
                               :indent "  "
                               :total nil
                               :unread nil)
                              (:name "personal"
                               :level 2
                               :expand t
                               :path "/account1/inbox/personal"
                               :indent "    "
                               :total nil
                               :unread nil)
                              (:name "work"
                               :level 2
                               :expand t
                               :path "/account1/inbox/work"
                               :indent "    "
                               :total nil
                               :unread nil))))))

(ert-deftest mu4e-maildirs-extension-children/all-from-submaildir ()
  "Tests fetching the children from a submaildir."
  (let* ((path-list '("/account1/*" "/account1/inbox/*"
                      "/account1/inbox/personal" "/account1/inbox/work"))
         (l (mapcar (lambda(path)
                      (mu4e-maildirs-extension-new-maildir path))
                    path-list))
         (m (mu4e-maildirs-extension-new-maildir "/account1/inbox/*"))
         (children (mu4e-maildirs-extension-children m l)))
    (should (equal children '((:name "personal"
                               :level 2
                               :expand t
                               :path "/account1/inbox/personal"
                               :indent "    "
                               :total nil
                               :unread nil)
                              (:name "work"
                               :level 2
                               :expand t
                               :path "/account1/inbox/work"
                               :indent "    "
                               :total nil
                               :unread nil))))))

(ert-deftest mu4e-maildirs-extension-member/exist ()
  "Tests fetching the member in list."
  (let* ((path-list '("/account1/*" "/account1/inbox/*"
                      "/account1/inbox/personal" "/account1/inbox/work"))
         (l (mapcar (lambda(path)
                      (mu4e-maildirs-extension-new-maildir path))
                    path-list))
         (path "/account1/inbox/*")
         (item (mu4e-maildirs-extension-member path l)))
    (should (equal item '(:name "inbox"
                          :level 1
                          :expand t
                          :path "/account1/inbox/*"
                          :indent "  "
                          :total nil
                          :unread nil)))))

(ert-deftest mu4e-maildirs-extension-member/not-exist ()
  "Tests fetching member that not exist in list."
  (let* ((path-list '("/account1/*" "/account1/inbox/*"
                      "/account1/inbox/personal" "/account1/inbox/work"))
         (l (mapcar (lambda(path)
                      (mu4e-maildirs-extension-new-maildir path))
                    path-list))
         (path "/account1/foo/*")
         (item (mu4e-maildirs-extension-member path l)))
    (should (equal item nil))))


(ert-deftest mu4e-maildirs-extension-is-parent-of/true ()
  "Tests checking if a maildir A is parent of B (true case)."
  (let* ((a (mu4e-maildirs-extension-new-maildir "/account1/a/*"))
         (b (mu4e-maildirs-extension-new-maildir "/account1/a/b"))
         (item (mu4e-maildirs-extension-is-parent-of a b)))
    (should item)))

(ert-deftest mu4e-maildirs-extension-is-parent-of/false ()
  "Tests checking if a maildir A is parent of B (false case)."
  (let* ((a (mu4e-maildirs-extension-new-maildir "/account1/a/*"))
         (b (mu4e-maildirs-extension-new-maildir "/account1/b"))
         (item (mu4e-maildirs-extension-is-parent-of a b)))
    (should (not item))))


(ert-deftest mu4e-maildirs-extension-parents/all-from-leaf ()
  "Tests fetching the parents of a leaf maildir."
  (let* ((path-list '("/account1/*" "/account1/inbox/*"
                      "/account1/inbox/personal" "/account1/inbox/work"))
         (l (mapcar (lambda(path)
                      (mu4e-maildirs-extension-new-maildir path))
                    path-list))
         (m (mu4e-maildirs-extension-new-maildir "/account1/inbox/work"))
         (m-account (mu4e-maildirs-extension-new-maildir "/account1/*"))
         (m-inbox (mu4e-maildirs-extension-new-maildir "/account1/inbox/*"))
         (parents (mu4e-maildirs-extension-parents m l)))
    (should (equal parents (list m-account
                                 m-inbox)))))

(ert-deftest mu4e-maildirs-extension-parents/all-from-middle ()
  "Tests fetching the parents of a middle maildir."
  (let* ((path-list '("/account1/*" "/account1/inbox/*"
                      "/account1/inbox/personal" "/account1/inbox/work"))
         (l (mapcar (lambda(path)
                      (mu4e-maildirs-extension-new-maildir path))
                    path-list))
         (m (mu4e-maildirs-extension-new-maildir "/account1/inbox/*"))
         (m-account (mu4e-maildirs-extension-new-maildir "/account1/*"))
         (parents (mu4e-maildirs-extension-parents m l)))
    (should (equal parents (list m-account)))))

(ert-deftest mu4e-maildirs-extension-parents/all-from-root ()
  "Tests fetching the parents of a root maildir."
  (let* ((path-list '("/account1/*" "/account1/inbox/*"
                      "/account1/inbox/personal" "/account1/inbox/work"))
         (l (mapcar (lambda(path)
                      (mu4e-maildirs-extension-new-maildir path))
                    path-list))
         (m (mu4e-maildirs-extension-new-maildir "/account1/*"))
         (parents (mu4e-maildirs-extension-parents m l)))
    (should (not parents))))


(ert-deftest mu4e-maildirs-extension-expanded/all-expanded ()
  "Tests fetching the expanded maildirs when all are expanded."
  (let* ((path-list '("/account1/*" "/account1/inbox/*"
                      "/account1/inbox/personal" "/account1/inbox/work"))
         (l (mapcar (lambda(path)
                      (mu4e-maildirs-extension-new-maildir path))
                    path-list))
         (expanded (mu4e-maildirs-extension-expanded l)))
    (should (equal expanded l))))

(ert-deftest mu4e-maildirs-extension-expanded/one-level-expanded ()
  "Tests fetching the expanded maildirs when all are expanded."
  (let* ((path-list '("/account1/*" "/account1/inbox/*"
                      "/account1/inbox/personal" "/account1/inbox/work"))
         (l (mapcar (lambda(path)
                      (mu4e-maildirs-extension-new-maildir path))
                    path-list))
         (expanded nil)
         (m-account (mu4e-maildirs-extension-new-maildir "/account1/*"))
         (m-inbox (mu4e-maildirs-extension-new-maildir "/account1/inbox/*"))
         (m (mu4e-maildirs-extension-member "/account1/inbox/*" l)))
    (setq m (plist-put m :expand nil))
    (setq m-inbox (plist-put m-inbox :expand nil))
    (setq expanded (mu4e-maildirs-extension-expanded l))

    (should (equal expanded (list m-account
                                  m-inbox)))))

(ert-deftest mu4e-maildirs-extension-prefix/collapsed ()
  "Tests the maildir collapsed prefix."
  (let* ((path-list '("/account1/*" "/account1/inbox"))
         (mu4e-maildirs-extension-maildirs
          (mapcar (lambda(path)
                    (mu4e-maildirs-extension-new-maildir path))
                  path-list))
         (m (mu4e-maildirs-extension-member "/account1/*"
                                            mu4e-maildirs-extension-maildirs))
         (prefix nil))
    (setq m (plist-put m :expand nil))
    (setq prefix (mu4e-maildirs-extension-update-maildir-prefix m))
    (should (equal prefix mu4e-maildirs-extension-maildir-collapsed-prefix))))

(ert-deftest mu4e-maildirs-extension-prefix/expanded ()
  "Tests the maildir expanded prefix."
  (let* ((path-list '("/account1/*" "/account1/inbox"))
         (mu4e-maildirs-extension-maildirs
          (mapcar (lambda(path)
                    (mu4e-maildirs-extension-new-maildir path))
                  path-list))
         (m (mu4e-maildirs-extension-member "/account1/*"
                                            mu4e-maildirs-extension-maildirs))
         (prefix (mu4e-maildirs-extension-update-maildir-prefix m)))
    (should (equal prefix mu4e-maildirs-extension-maildir-expanded-prefix))))

(ert-deftest mu4e-maildirs-extension-prefix/default ()
  "Tests the maildir default prefix."
  (let* ((path-list '("/account1/*" "/account1/inbox"))
         (mu4e-maildirs-extension-maildirs
          (mapcar (lambda(path)
                    (mu4e-maildirs-extension-new-maildir path))
                  path-list))
         (m (mu4e-maildirs-extension-member "/account1/inbox"
                                            mu4e-maildirs-extension-maildirs))
         (prefix (mu4e-maildirs-extension-update-maildir-prefix m)))
    (should (equal prefix mu4e-maildirs-extension-maildir-default-prefix))))

(provide 'mu4e-maildirs-extension-test)
;;; mu4e-maildirs-extension-test.el ends here
