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

;; namespace

(defun blog-admin-backend-build-datasource (keyword)
  "Build data source from backend defined"
  (let* ((blog-backend (plist-get blog-admin-backend-plist keyword))
         (scan-posts (plist-get blog-backend :scan-posts-func))
         (read-info  (plist-get blog-backend :read-info-func))
         )
    (mapcar (lambda (file)
              "Convert info into datesource"
              (let ((info (funcall read-info file)))
                (list (plist-get info :date) (plist-get info :tags) (plist-get info :publish) (plist-get info :title) file)
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
    (:tags "TAGS" nil nil)
    (:title "TITLE" nil nil)))

(defun blog-admin-backend--hexo-scan-posts ()
  "Scan posts of hexo"
  (apply 'append
         (mapcar
          (lambda (append-path)
            "scan files with append-path"
            (directory-files (blog-admin-backend--full-path append-path) t "^.*\\.org$"))
          '("source/_posts" "source/_drafts")
          )))

(defun blog-admin-backend--hexo-read-info (post)
  "Read info of hexo post"
  (let* ((info (blog-admin-backend--org-property-list post 'hexo-org))
         (datetime-str (plist-get info :date))
         )
    ;; read if publish
    (if (s-starts-with?
         (blog-admin-backend--full-path "source/_posts") post)
        ;; then
        (plist-put info :publish "YES")
      ;; else
      (plist-put info :publish "NO")
      )
    (plist-put info :date (blog-admin-backend--format-datetime datetime-str))
    info
    ))

(blog-admin-backend-define 'hexo
         '(:scan-posts-func
           blog-admin-backend--hexo-scan-posts
           :read-info-func
           blog-admin-backend--hexo-read-info
           ))


(provide 'blog-admin-backend)
;;; blog-admin-backend.el ends here
