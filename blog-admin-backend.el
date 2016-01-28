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


(defvar blog-admin-backend-plist nil
  "Backend blog-admin-backend-plist")

(defvar blog-admin-backend-path nil
  "Blog path in filesystem")

(defvar blog-admin-backend-type :hexo
  "Type of blog backend, options :hexo")

;; namespace

(defun blog-admin-backend-build-datasource (keyword)
  "Build data source from backend defined"
  (let* ((blog-backend (plist-get blog-admin-backend-plist keyword))
         (scan-posts (plist-get blog-backend :scan-posts))
         (read-info  (plist-get blog-backend :read-info))
         )
    (mapcar (lambda (file)
              "Convert info into datesource"
              (let ((info (funcall read-info file)))
                (list (plist-get info :date) (plist-get info :category) (plist-get info :title) file)
                )
              ) (funcall scan-posts))
    ))

(defun blog-admin-backend-define (keyword backend)
  "Put backend blog-admin-backend-define into blog-admin-backend-plist"
  (setq blog-admin-backend-plist (plist-put blog-admin-backend-plist keyword backend)))

;; org
(defun blog-admin-backend--org-property-list (filename org-backend)
  (if filename
      (with-temp-buffer
        (insert-file-contents filename)
        (org-mode)
        (org-export-get-environment org-backend))))

;; hexo define
(org-export-define-derived-backend 'hexo-org 'html
  :options-alist
  '((:date "DATE" nil nil)
    (:category "CATEGORIES" nil nil)
    (:title "TITLE" nil nil)))

(defun blog-admin-backend--hexo-scan-posts ()
  "Scan posts of hexo"
  (directory-files
   (expand-file-name (concat (file-name-as-directory blog-admin-backend-path) "/source/_posts"))
   t "^.*\\.org$"))

(defun blog-admin-backend--hexo-read-info (post)
  "Read info of hexo post"
  (blog-admin-backend--org-property-list post 'hexo-org)
  )

(blog-admin-backend-define :hexo
                           '(:scan-posts
                             blog-admin-backend--hexo-scan-posts
                             :read-info
                             blog-admin-backend--hexo-read-info
                             ))


(provide 'blog-admin-backend)
;;; blog-admin-backend.el ends here
