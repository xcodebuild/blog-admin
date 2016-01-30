;;; blog-admin-backend-org-page.el --- org-page backend for blog-admin  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  

;; Author:  <codefalling@localhost>
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
(require 'blog-admin-backend)
(require 'f)

(defvar blog-admin-backend-org-page-drafts "_drafts")

;; work with org-page
(eval-after-load 'org-page
  (add-to-list 'op/category-ignore-list "_drafts"))

(defun blog-admin-backend-org-page--categories ()
  "Return categories of org-page"
  (let ((dir-list (f-directories blog-admin-backend-path)))
    (dolist (x op/category-ignore-list)
      (delete x dir-list))))

(defun blog-admin-backend-org-page--scan-posts ()
  "Scan posts of org-page"
  (apply 'append
         (mapcar
          (lambda (append-path)
            "scan files with append-path"
            (directory-files (blog-admin-backend--full-path append-path) t "^.*\\.org$"))
          (append (blog-admin-backend-org-page--categories) blog-admin-backend-org-page-drafts)
          )
         ))

(defun blog-admin-backend-org-page--is-in-drafts? (post)
  "Return whether is post in drafts"
  (s-starts-with?
   (blog-admin-backend--full-path blog-admin-backend-org-page-drafts) post))

(defun blog-admin-backend-org-page--read-dir-in-ido ()
  "Return org-page category from id"
  (ido-completing-read "Category"
                       (blog-admin-backend-org-page--categories)))

(defun blog-admin-backend-org-page--file-path (name in-drafts? category)
  (f-join (blog-admin-backend--full-path
           (if in-drafts? blog-admin-backend-hexo-drafts-dir
             category))
          name))

(defun blog-admin-backend-org-page--exchange-place (path category)
  "Drafts->posts, posts->drafts"
  (if (f-exists? path)
      (let* ((name (f-filename path)))
        (if (blog-admin-backend-hexo--is-in-drafts? path)
            ;; drafts->posts
            (f-move (blog-admin-backend-hexo--file-path name t)
                    (blog-admin-backend-hexo--file-path name nil))
          ;; posts->drafts
          (f-move (blog-admin-backend-hexo--file-path name nil)
                  (blog-admin-backend-hexo--file-path name t))
          ))))

(defun blog-admin-backend-org-page--read-info (post)
  "Read info of org-page post"
  (let ((info (blog-admin-backend--read-org-info post)))
    ;; read if publish
    (if (blog-admin-backend-org-page--is-in-drafts? post)
        (plist-put info :publish "NO")
      (plist-put info :publish "YES")
      )
    ;; format datetime
    (plist-put info :date (blog-admin-backend--format-datetime (plist-get info :date)))
    ))


(blog-admin-backend-define 'org-page
                           '(:scan-posts-func
                             blog-admin-backend-org-page--scan-posts
                             :read-info-func
                             blog-admin-backend-org-page--read-info
                             :publish-unpublish-func
                             blog-admin-backend-hexo--publish-or-unpublish
                             :new-post-func
                             blog-admin-backend-hexo-new-post
                             ))

(provide 'blog-admin-backend-org-page)
;;; blog-admin-backend-org-page.el ends here
