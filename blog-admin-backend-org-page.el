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

;; Work in progress now
;;

;;; Code:
(require 'blog-admin-backend)
(require 'f)

(defvar blog-admin-backend-org-page-drafts "_drafts"
  "Default drafts directory for org-page")

(defvar blog-admin-backend-org-page-template-org-post "#+TITLE: New Post
#+DATE: %s
#+TAGS:
#+CATEGORIES:
#+OPTIONS:     H:3 num:nil toc:nil \\n:nil ::t |:t ^:nil -:nil f:t *:t <:t
"
  "template for org-page's org post")


;; work with org-page
(eval-after-load 'org-page
  (lambda ()
    (add-to-list 'op/category-ignore-list "_drafts")))

(defun blog-admin-backend-org-page--categories ()
  "Return categories of org-page"
  (mapcar
   'f-filename
   (f-directories
    blog-admin-backend-path
    (lambda (dir) (not (member (f-filename dir) (append op/category-ignore-list '(".git")))))
    )))

(defun blog-admin-backend-org-page--scan-posts ()
  "Scan posts of org-page"
  (apply 'append
         (mapcar
          (lambda (append-path)
            "scan files with append-path"
            (directory-files (blog-admin-backend--full-path append-path) t "^.*\\.org$"))
          (append (blog-admin-backend-org-page--categories) (list blog-admin-backend-org-page-drafts))
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
           (if in-drafts? blog-admin-backend-org-page-drafts
             category))
          name))

(defun blog-admin-backend-org-page--exchange-place (path category)
  "Drafts->posts, posts->drafts"
  (if (f-exists? path)
      (let* ((name (f-filename path)))
        (if (blog-admin-backend-org-page--is-in-drafts? path)
            ;; drafts->posts
            (f-move path
                    (blog-admin-backend-org-page--file-path name nil category))
          ;; posts->drafts
          (f-move path
                  (blog-admin-backend-org-page--file-path name t category))
          ))))

(defun blog-admin-backend-org-page--publish-or-unpublish ()
  "Switch between publish and drafts"
  (interactive)
  (let* ((post (blog-admin--table-current-file))
         (dirpath (f-no-ext post)))
    (if (blog-admin-backend-org-page--is-in-drafts? post)
        (progn (let ((category (blog-admin-backend-org-page--read-dir-in-ido)))
                 (blog-admin-backend-org-page--exchange-place post category)
                 (blog-admin-backend-org-page--exchange-place dirpath category)))
      (progn
        (blog-admin-backend-org-page--exchange-place post nil)
        (blog-admin-backend-org-page--exchange-place dirpath nil))
      )
    (blog-admin-refresh)
    ))


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

(defun blog-admin-backend-org-page-new-post (filename)
  "New org-page post"
  (interactive "sPost's filename(new-post.org etc):")
  (if (s-ends-with? ".org" filename)
      (progn
        (if blog-admin-backend-new-post-in-drafts
            (progn
              (if blog-admin-backend-new-post-with-same-name-dir
                  (f-mkdir (blog-admin-backend-org-page--file-path (f-no-ext filename) t nil)))
              (find-file (blog-admin-backend-org-page--file-path filename t nil))
              )
          (let ((categroy (blog-admin-backend-org-page--read-dir-in-ido)))
            (if blog-admin-backend-new-post-with-same-name-dir
                (f-mkdir (blog-admin-backend-org-page--file-path (f-no-ext filename) t categroy)))
            (find-file (blog-admin-backend-org-page--file-path filename t categroy))
            )
          )
        (insert
         (format
          blog-admin-backend-org-page-template-org-post
          (format-time-string "%Y-%m-%d" (current-time))
          ))
        (save-buffer)
        (kill-buffer)
        (blog-admin-refresh))
    (message "Post's filename must end with .org")
    ))


(blog-admin-backend-define 'org-page
                           '(:scan-posts-func
                             blog-admin-backend-org-page--scan-posts
                             :read-info-func
                             blog-admin-backend-org-page--read-info
                             :publish-unpublish-func
                             blog-admin-backend-org-page--publish-or-unpublish
                             :new-post-func
                             blog-admin-backend-org-page-new-post
                             ))

(provide 'blog-admin-backend-org-page)
;;; blog-admin-backend-org-page.el ends here
