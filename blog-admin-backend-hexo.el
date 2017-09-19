;;; blog-admin-backend-hexo.el --- hexo backend for blog-admin  -*- lexical-binding: t; -*-
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

(require 'ox)
(require 'blog-admin-backend)
(require 'names)

(define-namespace blog-admin-backend-hexo-

(defvar template-org-post "#+TITLE: %s
#+DATE: %s
#+LAYOUT: post
#+TAGS:
#+CATEGORIES:
"
  "template for hexo's org post")

(defvar template-md-post "title: %s
date: %s
tags:
categories:
---
"
  "template for hexo's markdown post")

(defvar posts-dir "source/_posts")
(defvar drafts-dir "source/_drafts")
(defvar config-file "_config.yml"
  "filename for nikola configuration file")

;; hexo define

(defun -scan-posts ()
  "Scan posts of hexo"
  (apply 'append
         (mapcar
          (lambda (append-path)
            "scan files with append-path"
            (directory-files (blog-admin-backend--full-path append-path) t "^.*\\.\\(org\\|md\\|markdown\\)$"))
          (list posts-dir drafts-dir)
          )))

(defun -is-in-drafts? (post)
  "Return whether is post in drafts"
  (s-starts-with?
   (blog-admin-backend--full-path drafts-dir) post))

(defun -read-info (post)
  "Read info of hexo post"
  (let ((info (if (s-ends-with? ".org" post)
                  ;; org post
                  (blog-admin-backend--read-org-info post)
                ;; markdown post
                (blog-admin-backend--read-md-info post))))
    ;; read if publish
    (if (-is-in-drafts? post)
        ;; then
        (plist-put info :publish "NO")
      ;; else
      (plist-put info :publish "YES")
      )
    ;; format datetime
    (plist-put info :date (blog-admin-backend--format-datetime (plist-get info :date)))
    ))

(defun -file-path (name in-drafts?)
  (f-join (blog-admin-backend--full-path
           (if in-drafts? drafts-dir
             posts-dir))
          name))

(defun -exchange-place (path)
  "Drafts->posts, posts->drafts"
  (if (f-exists? path)
      (let* ((name (f-filename path)))
        (if (-is-in-drafts? path)
            ;; drafts->posts
            (f-move path
                    (-file-path name nil))
          ;; posts->drafts
          (f-move path
                  (-file-path name t))
          ))))

(defun -publish-or-unpublish ()
  "Switch between publish and drafts"
  (interactive)
  (let* ((post (blog-admin--table-current-file))
         (dirpath (f-no-ext post)))
    (-exchange-place post)
    (-exchange-place dirpath)
    (blog-admin-refresh)
    ))

(defun -duplicate ()
  "Duplicate current post"
  (interactive)
  (let* ((post (blog-admin--table-current-file))
         (post-copy (concat (concat (f-no-ext post) "-copy.") (f-ext post)) ))
    (f-copy post post-copy))
  (blog-admin-refresh))

(defun new-post (filename)
  "New hexo post"
  (interactive "sPost's filename(new-post.org, new-post.md etc):")
  (if (or (s-ends-with? ".org" filename) (s-ends-with? ".md" filename))
      (let ((file-path (-file-path filename blog-admin-backend-new-post-in-drafts)))
        (if blog-admin-backend-new-post-with-same-name-dir
            (f-mkdir (f-no-ext file-path)))
        (find-file file-path)
        (insert
         (format
          (if (s-ends-with? ".org" filename) template-org-post template-md-post)
          (f-no-ext filename)
          (format-time-string "%Y-%m-%d" (current-time))
          ))
        (save-buffer)
        (kill-buffer)
        (blog-admin-refresh)
        (run-hook-with-args 'blog-admin-backend-after-new-post-hook file-path)
        )
    (message "Post's filename must end with .org or .md!")
    ))

(defun build-site ()
  "Build the site."
  (interactive)
  (let ((command (format
                  "cd %s && hexo --config %s generate &"
                  blog-admin-backend-path config-file)))
    (shell-command command)))

(defun deploy-site ()
  "Deploy the site."
  (interactive)
  (let ((command (format
                  "cd %s && hexo --config %s deploy --generate &"
                  blog-admin-backend-path config-file)))
    (shell-command command)))

(defun open-site-config ()
  "Open the config file for hexo"
  (interactive)
  (find-file (blog-admin-backend--full-path config-file)))

(blog-admin-backend-define 'hexo
                           `(:scan-posts-func
                             ,#'-scan-posts
                             :read-info-func
                             ,#'-read-info
                             :publish-unpublish-func
                             ,#'-publish-or-unpublish
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

) ;; namespace blog-admin-backend-hexo end here

(provide 'blog-admin-backend-hexo)
;;; blog-admin-backend-hexo.el ends here
