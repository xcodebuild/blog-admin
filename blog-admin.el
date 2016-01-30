;;; blog-admin.el --- Blog admin for emacs with hexo supported  -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author:  code.falling@gmail.com
;; Keywords: tools, blog, org

;; Version: 0.1
;; Package-Requires: ((org "8.0") (ctable "0.1.1") (s "1.10.0") (f "0.17.3"))

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
(require 'blog-admin-backend-hexo)
(require 'blog-admin-backend-org-page)

(setq blog-admin-backend-path "~/blog/")


(defvar blog-admin-mode-buffer nil
  "Main buffer of blog-admin")

(defvar blog-admin-mode-hook nil
  "Hooks for blog-admin-mode")

;; keymap
(defvar blog-admin-mode-map nil
  "Keymap for blog-admin-mode")

(defvar blog-admin-table nil
  "blog admin summary table")

(defun blog-admin--merge-keymap (keymap1 keymap2)
  (append keymap1
          (delq nil
                (mapcar
                 (lambda (x)
                   (if (or (not (consp x))
                           (assoc (car x) keymap1))
                       nil x))
                 keymap2))))


;; map
(defun blog-admin-load-map ()
  (setq blog-admin-mode-map (make-sparse-keymap))
  (define-key blog-admin-mode-map (kbd "<up>") 'ctbl:navi-move-up)
  (define-key blog-admin-mode-map (kbd "<down>") 'ctbl:navi-move-down)

  (define-key blog-admin-mode-map "d" 'blog-admin-delete-post)
  (define-key blog-admin-mode-map "s" (plist-get (blog-admin-backend-get-backend) :publish-unpublish-func))
  (define-key blog-admin-mode-map "w" (plist-get (blog-admin-backend-get-backend) :new-post-func))
  (define-key blog-admin-mode-map "r" 'blog-admin-refresh)
  (setq blog-admin-mode-map
        (blog-admin--merge-keymap blog-admin-mode-map ctbl:table-mode-map)))

;; table

(defun blog-admin--table-current-file ()
  (nth 3 (ctbl:cp-get-selected-data-row blog-admin-table))
  )

(defun blog-admin--table-click ()
  "Click event for table"
  (find-file (blog-admin--table-current-file)))

(defun blog-admin--table-header (&optional title)
  (concat
   (format "%s\n" (or title "Blog"))
   (mapconcat
    'identity
    (blog-admin--table-help
     (cl-remove-duplicates
      (mapcar 'cdr (cdr blog-admin-mode-map)))
     blog-admin-mode-map)
    "\n")
   "\n\n\n"))

(defun blog-admin--table-help (symbols &optional keymap)
  (let (symbol keysym keystr docstr summary-list)
    (while (setq symbol (car symbols))
      (setq keysym (where-is-internal symbol (or keymap (current-local-map)) nil)
            keystr (if keysym (mapconcat 'key-description keysym ",") "No keybind")
            docstr (documentation symbol))
      (if docstr
          (setq summary-list (cons (format "%10s ... %s"
                                           keystr
                                           (car (split-string docstr "\n"))
                                           )
                                   summary-list)))
      (setq symbols (cdr symbols)))
    summary-list))

(defun blog-admin--table-build (contents keymap)
  (insert (blog-admin--table-header))
  (let ((param (copy-ctbl:param ctbl:default-rendering-param)))
    (setf (ctbl:param-fixed-header param) t)
    (save-excursion (setq blog-admin-table (ctbl:create-table-component-region
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
                                          :title "Publish"
                                          :align 'left
                                          :sorter 'ctbl:sort-string-lessp)
                                         (make-ctbl:cmodel
                                          :title "Title"
                                          :align 'left
                                          :min-width 40
                                          :max-width 140)
                                         )))))

    (ctbl:cp-add-click-hook blog-admin-table 'blog-admin--table-click)
    ))


(defun blog-admin-refresh ()
  "Refresh *Blog*"
  (interactive)
  (when blog-admin-mode-buffer
    (kill-buffer blog-admin-mode-buffer)
    (blog-admin-start)))

(defun blog-admin-delete-post ()
  "Delete post"
  (interactive)
  (let ( (file-path (blog-admin--table-current-file)) )
    (if (y-or-n-p (format "Do you really want to delete %s" file-path))
        (progn
          (delete-file file-path)
          ;; remove asset directory if exist
          (let ((dir-path (file-name-sans-extension file-path)))
            (if (file-exists-p dir-path)
                (delete-directory dir-path t))
            )
          (blog-admin-refresh)
          ))))

;; main

;;;###autoload
(defun blog-admin-start ()
  "Blog admin"
  (interactive)
  (setq blog-admin-mode-buffer (get-buffer-create "*Blog*"))
  (switch-to-buffer blog-admin-mode-buffer)
  (setq buffer-read-only nil)
  (erase-buffer)
  (blog-admin--table-build (blog-admin-backend-build-datasource blog-admin-backend-type) blog-admin-mode-map)
  (blog-admin-mode)
  )

(define-derived-mode blog-admin-mode nil "Blog"
  "Major mode for blog-admin."
  (set (make-local-variable 'buffer-read-only) t)
  (blog-admin-load-map))

(provide 'blog-admin)
;;; blog-admin.el ends here
