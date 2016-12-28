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

(define-namespace blog-admin-backend-org-page-

(defvar drafts "_drafts"
  "Default drafts directory for org-page")

(defvar template-org-post "#+TITLE: %s
#+DATE: %s
#+TAGS:
"
  "template for org-page's org post")

(defvar config-file nil
  "path to the file where org-page is configured")

;; work with org-page
(eval-after-load 'org-page
  (lambda ()
    (add-to-list 'op/category-ignore-list "_drafts")))

(defun -categories ()
  "Return categories of org-page"
  (mapcar
   'f-filename
   (f-directories
    blog-admin-backend-path
    (lambda (dir) (not (member (f-filename dir) (append op/category-ignore-list '(".git")))))
    )))

(defun -scan-posts ()
  "Scan posts of org-page"
  (if (not (f-exists? (blog-admin-backend--full-path drafts)))
      (f-mkdir (blog-admin-backend--full-path drafts)))
  (apply 'append
         (mapcar
          (lambda (append-path)
            "scan files with append-path"
            (directory-files (blog-admin-backend--full-path append-path) t "^.*\\.org$"))
          (append (-categories) (list drafts))
          )
         ))

(defun -is-in-drafts? (post)
  "Return whether is post in drafts"
  (s-starts-with?
   (blog-admin-backend--full-path drafts) post))

(defun -read-dir-in-ido ()
  "Return org-page category from id"
  (ido-completing-read "Category"
                       (-categories)))

(defun -file-path (name in-drafts? category)
  (f-join (blog-admin-backend--full-path
           (if in-drafts? drafts
             category))
          name))

(defun -exchange-place (path category)
  "Drafts->posts, posts->drafts"
  (if (f-exists? path)
      (let* ((name (f-filename path)))
        (if (-is-in-drafts? path)
            ;; drafts->posts
            (f-move path
                    (-file-path name nil category))
          ;; posts->drafts
          (f-move path
                  (-file-path name t category))
          ))))

(defun publish-or-unpublish ()
  "Switch between publish and drafts"
  (interactive)
  (let* ((post (blog-admin--table-current-file))
         (dirpath (f-no-ext post)))
    (if (-is-in-drafts? post)
        (progn (let ((category (-read-dir-in-ido)))
                 (-exchange-place post category)
                 (-exchange-place dirpath category)))
      (progn
        (-exchange-place post nil)
        (-exchange-place dirpath nil))
      )
    (blog-admin-refresh)
    ))

(defun -duplicate ()
  "Duplicate current post"
  (interactive)
  (let* ((post (blog-admin--table-current-file))
         (post-copy (concat (concat (f-no-ext post) "-copy.") (f-ext post)) ))
    (f-copy post post-copy))
  (blog-admin-refresh))



(defun -read-info (post)
  "Read info of org-page post"
  (let ((info (blog-admin-backend--read-org-info post)))
    ;; read if publish
    (if (-is-in-drafts? post)
        (plist-put info :publish "NO")
      (plist-put info :publish "YES")
      )
    ;; format datetime
    (plist-put info :date (blog-admin-backend--format-datetime (plist-get info :date)))
    ))

(defun new-post (filename)
  "New org-page post"
  (interactive "sPost's filename(new-post.org etc):")
  (if (s-ends-with? ".org" filename)
      (progn
        ;; mkdir _drafts if not exists
        (if (not
             (f-exists? (f-join blog-admin-backend-path drafts)))
            (f-mkdir (f-join blog-admin-backend-path drafts)))

        (let ((file-path  (if blog-admin-backend-new-post-in-drafts
                              (-file-path filename t nil)
                            (-file-path filename t (-read-dir-in-ido))
                            )))
          (if blog-admin-backend-new-post-with-same-name-dir
              (f-mkdir (f-no-ext file-path)))
          (find-file file-path)
          (insert
           (format
            template-org-post
            (f-no-ext filename)
            (format-time-string "%Y-%m-%d" (current-time))
            ))
          (save-buffer)
          (kill-buffer)
          (blog-admin-refresh)
          (run-hook-with-args 'blog-admin-backend-after-new-post-hook file-path)
          )
        )
    (message "Post's filename must end with .org")
    ))

(defun build-site ()
  "Build the site."
  (interactive)
  (require 'org-page)
  (op/do-publication t nil op/site-preview-directory))

(defun deploy-site ()
  "Deploy the site."
  (interactive)
  (require 'org-page)
  (op/do-publication))

(defun open-site-config ()
  "Open the config file for hexo"
  (interactive)
  (find-file (or config-file user-init-file)))

(blog-admin-backend-define 'org-page
                           `(:scan-posts-func
                             ,#'-scan-posts
                             :read-info-func
                             ,#'-read-info
                             :publish-unpublish-func
                             ,#'publish-or-unpublish
                             :duplicate
                             ,#'-duplicate
                            :new-post-func
                             ,#'new-post
                             :build-site-func
                             ,#'build-site
                             :deploy-site-func
                             ,#'deploy-site
                             :open-site-config-func
                             ,#'open-site-config
                             ))

) ;; namespace blog-admin-backend-org-page end here

(provide 'blog-admin-backend-org-page)
;;; blog-admin-backend-org-page.el ends here
