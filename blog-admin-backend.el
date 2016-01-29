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
(require 's)

(defvar blog-admin-backend-plist nil
  "Backend blog-admin-backend-plist")

(defvar blog-admin-backend-path nil
  "Blog path in filesystem")

(defvar blog-admin-backend-type 'hexo
  "Type of blog backend, options :hexo")

;; hexo defvar
(defvar blog-admin-backend-hexo-default-file-type 'markdown
  "Default file type of new post, 'markdown or 'org ")
(defvar blog-admin-backend-hexo-default-post-publish nil
  "`nil`->new post will be publish, `t`-> new post will be placed in draft")

(defun blog-admin-backend-build-datasource (keyword)
  "Build data source from backend defined"
  (let* ((blog-backend (plist-get blog-admin-backend-plist keyword))
         (scan-posts (plist-get blog-backend :scan-posts-func))
         (read-info  (plist-get blog-backend :read-info-func))
         )
    (mapcar (lambda (file)
              "Convert info into datesource"
              (let ((info (funcall read-info file)))
                ;; put file path at last for read from table
                (list (plist-get info :date) (plist-get info :publish) (plist-get info :title) file)
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

;; helper
(defun blog-admin-backend--full-path (append-path)
  "Return full absolute path base on `blog-admin-backend-path`"
  (expand-file-name
   (concat
    (file-name-as-directory blog-admin-backend-path)
    append-path)))

(defun blog-admin-backend--format-datetime (datetime-str)
  (let ((l (parse-time-string datetime-str)))
    (format-time-string "%Y-%m-%d" (encode-time 0 0 0 (nth 3 l) (nth 4 l) (nth 5 l))))
  )

;; hexo define
(org-export-define-derived-backend 'hexo-org 'html
  :options-alist
  '((:date "DATE" nil nil)
    (:title "TITLE" nil nil)))

(defun blog-admin-backend--hexo-scan-posts ()
  "Scan posts of hexo"
  (apply 'append
         (mapcar
          (lambda (append-path)
            "scan files with append-path"
            (directory-files (blog-admin-backend--full-path append-path) t "^.*\\.\\(org\\|md\\|markdown\\)$"))
          '("source/_posts" "source/_drafts")
          )))

(defun blog-admin-backend--hexo-read-info (post)
  "Read info of hexo post"
  (let ((info (if (s-ends-with? ".org" post)
                  ;; org post
                  (blog-admin-backend--hexo-read-org-info post)
                ;; markdown post
                (blog-admin-backend--hexo-read-md-info post))))
    ;; read if publish
    (if (s-starts-with?
         (blog-admin-backend--full-path "source/_posts") post)
        ;; then
        (plist-put info :publish "YES")
      ;; else
      (plist-put info :publish "NO")
      )
    ;; format datetime
    (plist-put info :date (blog-admin-backend--format-datetime (plist-get info :date)))
    ))

(defun blog-admin-backend--hexo-read-md-info (post)
  "Read info of hexo markdown post"
  (with-temp-buffer
    (insert-file-contents post)
    (let ((info nil))
      (setq info (plist-put info :title
                            (s-chop-prefix "title: " (car (s-match "^title: .*?\n" (buffer-string))))))
      (plist-put info :date
                 (s-chop-prefix "date: " (car (s-match "^date: .*?\n" (buffer-string)))))
      info
      )
    ))

(defun blog-admin-backend--hexo-read-org-info (post)
  "Read info of hexo org post"
  (blog-admin-backend--org-property-list post 'hexo-org))

(blog-admin-backend-define 'hexo
         '(:scan-posts-func
           blog-admin-backend--hexo-scan-posts
           :read-info-func
           blog-admin-backend--hexo-read-info
           ))


(provide 'blog-admin-backend)
;;; blog-admin-backend.el ends here
