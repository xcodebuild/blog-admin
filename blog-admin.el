;;; blog-admin.el --- Blog admin for emacs with hexo supported  -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author:  code.falling@gmail.com
;; Keywords: tools

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
(require 'cl-lib)
(require 'org)
(require 'ctable)
(require 'names)
;; (require 'blog-admin-backend)


(define-namespace blog-admin- :global
;; namespace blog-admin

(defvar mode-buffer nil
  "Main buffer of blog-admin")

;; keymap
(defvar mode-map nil
  "Keymap for blog-admin-mode")

;; table
(defun build-table (contents keymap)
  (let ((param (copy-ctbl:param ctbl:default-rendering-param)))
    (setf (ctbl:param-fixed-header param) t)
    (ctbl:create-table-component-region
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
            )))))

;; main
:autoload
(defun start ()
  "Blog admin"
  (interactive)
  (setq mode-buffer (get-buffer-create "*Blog*"))
  (switch-to-buffer mode-buffer)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq a (backend-build-datasource :hexo))
  )

);; namespace blog-admin end here

(provide 'blog-admin)
;;; blog-admin.el ends here
