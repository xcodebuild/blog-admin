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
  "Backend plist"
  :group blog-admin)

(defvar path nil
  "Blog path in filesystem"
  :group blog-admin)

;; namespace

(defun build-datasource (keyword)
  "Build data source from backend defined"
  (let* ((blog-backend (plist-get plist keyword))
         (scan-posts (plist-get blog-backend :scan-posts))
         (read-info  (plist-get blog-backend :read-info))
         )
    (mapcar (lambda (file)
              (let ((info (funcall read-info file)))
                (list (plist-get info :date) (plist-get info :category) (plist-get info :title))
                )
              ) (funcall scan-posts))
    ))

(defun define (keyword backend)
  "Put backend define into blog-admin-backend-plist"
  (setq plist (plist-put plist keyword backend)))

;; org
(defun org-property-list (filename org-backend)
  (if filename
      (with-temp-buffer
        (insert-file-contents filename)
        (org-mode)
        (setq plist (org-export-get-environment org-backend))
        (setq plist (plist-put plist :input-file filename)))
    (setq plist (org-export-backend-options org-backend))
    plist))

;; hexo
(org-export-define-derived-backend 'hexo-org 'html
  :options-alist
  '((:date "DATE" nil nil)
    (:category "CATEGORIES" nil nil)
    (:title "TITLE" nil nil)))

(define :hexo
  '(
    :scan-posts
    (lambda () (directory-files
                (expand-file-name
                 path "source/_posts") t "^.*\\.org$"))
    :read-info
    (lambda (post)
      (org-property-list post 'hexo-org)
      )
    ))

  ) ;; namespace blog-admin-backend end here

(provide 'blog-admin-backend)
;;; blog-admin-backend.el ends here
