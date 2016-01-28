;;; blog-admin-backend.el --- backend for blog-admin  -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author: code.falling@gmail.com
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'names)

(define-namespace blog-admin-backend-

(defvar plist nil
  "Backend plist")

;; namespace

(defun build-datasource (keyword)
  "Build data source from backend defined"
  (let* ((blog-backend (plist-get plist keyword))
         (scan-posts (plist-get blog-backend :scan-posts))
         (read-date      (plist-get blog-backend :read-date))
         (read-title     (plist-get blog-backend :read-title))
         (read-category  (plist-get blog-backend :read-category))
         )
    (mapcar (lambda (file)
              ( (funcall read-date file) (funcall read-title file) (funcall read-category file) )
              ) (funcall scan-posts))
    ))

(defun define (keyword backend)
  "Put backend define into blog-admin-backend-plist"
  (setq plist (plist-put plist keyword backend)))

;; hexo
(define :hexo
  '(
    :scan-posts
    (lambda () (directory-files
                (expand-file-name
                 "~/blog/source/_posts") t "^[0-9].*\\.org$"))
    :read-date
    (lambda (post) "test")
    :read-title
    (lambda (post) "title")
    :read-category
    (lambda (post) "category")
    ))

  ) ;; namespace blog-admin-backend end here

(provide 'blog-admin-backend)
;;; blog-admin-backend.el ends here
