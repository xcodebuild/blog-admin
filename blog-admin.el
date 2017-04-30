;;; blog-admin.el --- Blog admin for emacs with hexo/org-page supported  -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author:  code.falling@gmail.com
;; Keywords: tools, blog, org, hexo, org-page

;; Version: 0.1
;; Package-Requires: ((ctable "0.1.1") (s "1.10.0") (f "0.17.3") (names "20151201.0") (cl-lib "0.5"))

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

;; (setq blog-admin-backend-path "~/blog")
;; (setq blog-admin-backend-type 'hexo)
;;

;;; Code:
(require 'org)
(require 'ctable)
(require 'names)
(require 'cl-lib)

(require 'blog-admin-backend-hexo)
(require 'blog-admin-backend-org-page)
(require 'blog-admin-backend-nikola)

(define-namespace blog-admin-
;; namespace blog-admin

(defvar mode-buffer nil
  "Main buffer of blog-admin")

(defvar mode-hook nil
  "Hooks for mode")

;; keymap
(defvar mode-map nil
  "Keymap for mode")

(defvar table nil
  "blog admin summary table")

(defconst -table-help
  "Blog

s   ... Switch between publish and drafts
d   ... Delete current post
c   ... Duplicate current post
w   ... Write new post
RET ... Open current post
r   ... Refresh blog-admin
B   ... Build site
C   ... Jump to the config
D   ... Deploy site
F   ... Filter and show only rows with keyword

"
  "Help of table")

(defvar show-help t)

(defun -merge-keymap (keymap1 keymap2)
  (append keymap1
          (delq nil
                (mapcar
                 (lambda (x)
                   (if (or (not (consp x))
                           (assoc (car x) keymap1))
                       nil x))
                 keymap2))))


;; map
(defun load-map ()
  (setq mode-map (make-sparse-keymap))
  (define-key mode-map (kbd "<up>") 'ctbl:navi-move-up)
  (define-key mode-map (kbd "<down>") 'ctbl:navi-move-down)

  (define-key mode-map "d" #'delete-post)
  (define-key mode-map "s" (plist-get (blog-admin-backend-get-backend) :publish-unpublish-func))
  (define-key mode-map "c" (plist-get (blog-admin-backend-get-backend) :duplicate))
  (define-key mode-map "w" (plist-get (blog-admin-backend-get-backend) :new-post-func))
  (define-key mode-map "r" #'refresh)
  (define-key mode-map "B" (plist-get (blog-admin-backend-get-backend) :build-site-func))
  (define-key mode-map "C" (plist-get (blog-admin-backend-get-backend) :open-site-config-func))
  (define-key mode-map "D" (plist-get (blog-admin-backend-get-backend) :deploy-site-func))
  (define-key mode-map "F" #'filter)
  (setq mode-map
        (-merge-keymap mode-map ctbl:table-mode-map)))

;; table

(defun -table-current-file ()
  (nth 3 (ctbl:cp-get-selected-data-row table))
  )

(defun -table-click ()
  "Click event for table"
  (find-file (-table-current-file)))



(defun -table-build ()
  (when show-help (insert -table-help))
  (let ((param (copy-ctbl:param ctbl:default-rendering-param)))
    (setf (ctbl:param-fixed-header param) t)
    (save-excursion (setq table (ctbl:create-table-component-region
                                 :param param
                                 :width  nil
                                 :height nil
                                 :keymap mode-map
                                 :model (-get-model))))

    (ctbl:cp-add-click-hook table #'-table-click)
    (ctbl:navi-goto-cell (ctbl:cell-id 0 0))
    ))

(defun -get-model (&optional filter-keyword)
  "Get table model (optionally only rows with FILTER-KEYWORD)."
  (let ((contents
         (blog-admin-backend-build-datasource blog-admin-backend-type)))
    ;; Filter the rows to drop all rows not containing the keyword.
    (when filter-keyword
      (setq contents
            (cl-remove-if-not (lambda (x)
                                ;; concatenate the row with | (dropping filename)
                                (let ((row (mapconcat #'identity (butlast x) "|")))
                                  (s-contains? filter-keyword row t)))
                              contents)))

    (make-ctbl:model
     :data contents
     :sort-state '(-1 2)
     :column-model
     (list (make-ctbl:cmodel
            :title "Date"
            :sorter 'ctbl:sort-string-lessp
            :min-width 10
            :align 'left)
           (make-ctbl:cmodel
            :title "Publish"
            :align 'left
            :sorter 'ctbl:sort-string-lessp)
           (make-ctbl:cmodel
            :title "Title"
            :align 'left
            :min-width 40
            :max-width 120)
           ))))

(defun delete-post ()
  "Delete post"
  (interactive)
  (let ( (file-path (-table-current-file)) )
    (if (y-or-n-p (format "Do you really want to delete %s" file-path))
        (progn
          (delete-file file-path)
          ;; remove asset directory if exist
          (let ((dir-path (file-name-sans-extension file-path)))
            (if (file-exists-p dir-path)
                (delete-directory dir-path t)))
          (refresh)
          ;; Move up and down to keep the cursor inside the table
          (unless (ignore-errors (ctbl:cursor-to-nearest-cell))
            (forward-line -1)
            (ctbl:navi-goto-cell (ctbl:cursor-to-nearest-cell)))))))

(defun refresh ()
  "Refresh *Blog*"
  (interactive)
  (ctbl:cp-set-model blog-admin-table (-get-model)))


(defun filter ()
  "Filter table based on user input"
  (interactive)
  (let* ((keyword (read-from-minibuffer "Search filter: ")))
    (ctbl:cp-set-model blog-admin-table (-get-model keyword))))

;; main

:autoload
(defun start ()
  "Blog admin"
  (interactive)
  (setq mode-buffer (get-buffer-create "*Blog*"))
  (switch-to-buffer mode-buffer)
  (setq-local default-directory blog-admin-backend-path)
  (setq buffer-read-only nil)
  (erase-buffer)
  (load-map)
  (-table-build)
  (mode)
  )

(define-derived-mode mode special-mode "Blog"
  "Major mode for blog-admin."
  (set (make-local-variable 'buffer-read-only) t))

) ;; namespace blog-admin end here

(provide 'blog-admin)
;;; blog-admin.el ends here
