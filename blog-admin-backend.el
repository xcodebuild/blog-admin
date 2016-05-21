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
(require 'f)
(require 'names)

(define-namespace blog-admin-backend-

(defvar plist nil
  "Backend plist")

(defvar path nil
  "Blog path in filesystem")

(defvar type 'hexo
  "Type of blog backend, options :hexo")

(defvar new-post-in-drafts nil
  "`nil`->new post will be publish, `t`-> new post will be placed in drafts")

(defvar new-post-with-same-name-dir t
  "`nil`->new post will be a single file, `t`-> new post will come with a same-name directory")

(defvar after-new-post-hook nil
  "Hook after new post create, should be execute at backend-*.el")

(defun build-datasource (keyword)
  "Build data source from backend defined"
  (let* ((blog-backend (plist-get plist keyword))
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

(defun define (keyword backend)
  "Put backend define into plist"
  (setq plist (plist-put plist keyword backend)))

(defun get-backend ()
  "Return backend"
  (plist-get plist type)
  )

;; org
;; helper
(defun -full-path (append-path)
  "Return full absolute path base on `path`"
  (expand-file-name
   (concat
    (file-name-as-directory path)
    append-path)))

(defun -format-datetime (datetime)
  (let*
      (
       (datetime-in-plist
        (if (not (stringp datetime))
            (plist-get (plist-get (car datetime) 'timestamp) :raw-value)) ;; orgmode 8.2.10 return a plist
        nil
        )
       (datetime-str (cond
                      ((eq datetime nil) "") ;; nil
                      ((stringp datetime-in-plist) datetime-in-plist)
                      ((stringp datetime) datetime)
                      (t (car datetime)) ;; some version return a list
                      ))
       (l (parse-time-string datetime-str)))
    (if (equal l
               '(nil nil nil nil nil nil nil nil nil)) ;; can't parse
        "Can't Parse"
      (format-time-string "%Y-%m-%d"
                          (encode-time 0 0 0 (nth 3 l) (nth 4 l) (nth 5 l))))))

  (defun -default (value default)
    (if value value default))

  (defun -read-org-info (post)
    "Read info of org post"
    (with-temp-buffer
      (insert-file-contents post)
      (let ((info nil))
        (setq info (plist-put info :title
                              (s-trim (-default
                                       (s-chop-prefix "#+TITLE:" (car (s-match "^#\\+TITLE:.*?\n" (buffer-string))))
                                       ""
                                       ))))
        (plist-put info :date
                   (s-trim (-default
                            (s-chop-prefix "#+DATE:" (car (s-match "^#\\+DATE:.*?\n" (buffer-string))))
                            "")))
        info
        )
      ))



  (defun -read-md-info (post)
    "Read info of markdown post"
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

  ) ;; namespace end here

(provide 'blog-admin-backend)
;;; blog-admin-backend.el ends here
