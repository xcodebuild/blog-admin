;;; blog-admin.el --- Blog admin for emacs with hexo supported  -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author:  code.falling@gmail.com
;; Keywords: tools, blog, org

;; Version: 0.1
;; Package-Requires: ((org "8.0") (ctable "0.1.1")

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
;; (setq blog-admin-backend-type :hexo)
;;

;;; Code:
(require 'org)
(require 'ctable)
;; (require 'blog-admin-backend)

(setq blog-admin-backend-path "~/blog/")


(defvar blog-admin-mode-buffer nil
  "Main buffer of blog-admin")

;; keymap
(defvar blog-admin-mode-map nil
  "Keymap for blog-admin-mode")

(defvar blog-admin-table nil
  "blog admin summary table")

;; table
(defun blog-admin-build-table (contents keymap)
  (let ((param (copy-ctbl:param ctbl:default-rendering-param)))
    (setf (ctbl:param-fixed-header param) t)
    (setq blog-admin-table (ctbl:create-table-component-region
                            :param param
                            :width  nil
                            :height nil
                            :keymap keymap
                            :model
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
                                    :title "Category"
                                    :align 'left
                                    :sorter 'ctbl:sort-string-lessp)
                                   (make-ctbl:cmodel
                                    :title "Title"
                                    :align 'left
                                    :min-width 40
                                    :max-width 140)
                                   ))))
    (ctbl:cp-add-click-hook
     blog-admin-table
     (lambda ()
       (message "test")
       (find-file (nth 4 (ctbl:cp-get-selected-data-row nil)))))
    ))

(defun blog-admin--merge-keymap (keymap1 keymap2)
  (append keymap1
          (delq nil
                (mapcar
                 (lambda (x)
                   (if (or (not (consp x))
                           (assoc (car x) keymap1))
                       nil x))
                 keymap2))))



(defun blog-admin-refresh ()
  "Refresh buffer."
  (interactive)
  (when blog-admin-mode-buffer
    (kill-buffer blog-admin-mode-buffer)
    (blog-admin-start)))

;; main

(defun blog-admin-start ()
  "Blog admin"
  (interactive)
  (setq blog-admin-mode-buffer (get-buffer-create "*Blog*"))
  (switch-to-buffer blog-admin-mode-buffer)
  (setq buffer-read-only nil)
  (erase-buffer)
  (blog-admin-build-table (blog-admin-backend-build-datasource :hexo) blog-admin-mode-map)
  )

(provide 'blog-admin)
;;; blog-admin.el ends here
