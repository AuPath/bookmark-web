;;; bookmark-web.el --- Bookmark web urls with the standard bookmark interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Marco Grassi

;; Author: Marco Grassi <marco.au.grassi98@protonmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'bookmark)
(require 'seq)

(defgroup bookmark-web nil
  "Bookmark web urls."
  :group 'convenience
  :prefix "bookmark-web")

(defcustom bookmark-web-get-url-function
  #'bookmark-web-get-url-empty
  "Function used to get url of web page.
Useful to redifine while using exwm or other window managers to
automate the grabbing of the url from a browser.  This url is
shown as a suggestion, it is always possible to enter a url
manually.  This function should return a list."
  :type 'function)

(defun bookmark-web-get-url-empty ()
  "By default the url is entered manually."
  nil)

;;;###autoload
(defun bookmark-web-add-bookmark ()
  "Save web url as bookmark."
  (interactive)
  (let* ((url (completing-read "Url: " (funcall bookmark-web-get-url-function)))
         (bookmark-name (completing-read "Bookmark name: " (list url))))
    (bookmark-web-store url bookmark-name)))

(defun bookmark-web-store (url bookmark-name)
  "Save web URL as bookmark with BOOKMARK-NAME."
  (if (assoc bookmark-name bookmark-alist)
      (user-error "%s is already bookmarked" bookmark-name)
    (bookmark-store bookmark-name
                    (list (cons 'filename url)
                          (cons 'handler #'bookmark-web-handler))
                    nil)))

;;;###autoload
(defun bookmark-web-handler (bm)
  "Handler for web bookmarks, opens bookmark BM with default browser."
  (browse-url (assoc-default 'filename (cdr bm))))

;;;###autoload
(defun bookmark-web-names ()
  "Return a list of names of all web bookmarks."
  (bookmark-maybe-load-default-file)
  (mapcar #'car (seq-filter (lambda (x) (eq #'bookmark-web-handler
                                            (alist-get 'handler (cdr x))))
                            bookmark-alist)))

(provide 'bookmark-web)
;;; bookmark-web.el ends here
