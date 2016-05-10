;;; blog-admin-backend-nikola.el --- nikola backend for blog-admin  -*- lexical-binding: t; -*-
;; Copyright (C) 2016

;; Author: punchagan@muse-amuse.in
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

(define-namespace blog-admin-backend-nikola-

(defvar posts-dir "posts")

(defvar executable nil)
(defvar -draft-posts nil)
(defvar -published-posts nil)


;; nikola define

(defun -scan-posts-by-type (&optional draft)
  "Scan posts of nikola by type -- published or draft."
  (let* ((flag (if draft "-d" "-P"))
         (resize-mini-windows nil) ;; Don't show output in mini-buffer
         (prefix (if draft "Draft" "Published"))
         (command (format
                   "cd %s && %s status %s"
                   blog-admin-backend-path
                   blog-admin-backend-nikola-executable
                   flag))
         (output-buffer-name "*blog-admin-backend-nikola-output*")
         (output (shell-command command output-buffer-name output-buffer-name))
         paths)
    (with-current-buffer output-buffer-name
      (goto-char (point-min))
      (delete-non-matching-lines (format "^%s: " prefix))
      (setq paths (mapcar
                   (lambda (x) (car (cdr x)))
                   (s-match-strings-all "source: \\(.*?\\))" (buffer-string))))

      (kill-buffer))
    (mapcar #'blog-admin-backend--full-path paths)))

(defun -scan-posts ()
  "Scan posts of nikola"
  (message "Scanning posts...")
  (setq -draft-posts (-scan-posts-by-type t))
  (setq -published-posts (-scan-posts-by-type))
  (message "Scanning posts... Done!")
  (append -draft-posts -published-posts))

(defun -is-in-drafts? (post)
  "Return whether is post in drafts"
  (member post -draft-posts))

(defun -read-nikola-info (post)
    "Read info of any nikola post"
    (with-temp-buffer
      (insert-file-contents post)
      (let ((info nil))
        (setq info (plist-put info :title
                              (s-chop-prefix ".. title: " (car (s-match "^\\.\\. title: .*?\n" (buffer-string))))))
        (plist-put info :date
                   (s-chop-prefix ".. date: " (car (s-match "^\\.\\. date: .*?\n" (buffer-string)))))
        info
        )
      ))

(defun -read-info (post)
  "Read info of nikola post"
  (let ((info (-read-nikola-info post)))
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

(defun -toggle-draft-tag (post)
  "Add or remove draft tag on a post"
  (let* ((flag (if (-is-in-drafts? post) "-r" "-a"))
         (resize-mini-windows nil) ;; Don't show output in mini-buffer
         (command (format
                   "cd %s && %s tags %s draft %s"
                   blog-admin-backend-path
                   blog-admin-backend-nikola-executable
                   flag
                   post)))
    (shell-command command)))

(defun -publish-or-unpublish ()
  "Switch between publish and drafts"
  (interactive)
  (let* ((post (blog-admin--table-current-file)))
    (-toggle-draft-tag post)
    (blog-admin-refresh)))

(defun new-post (title &optional paste-subtree import-from)
  "New nikola post"
  (interactive "MTitle: ")
  (let* ((tags (if blog-admin-backend-new-post-in-drafts "--tags=draft" ""))
         (command (format
                   "cd %s && %s new_post %s -t \"%s\""
                   blog-admin-backend-path executable tags title))
         (command- (or (and import-from (format "%s -i %s" command import-from))
                       command))
         (output (shell-command-to-string command-))
         (path (save-match-data
                 (string-match "\n.*text is at:\s*\\(.*\\)\n" output)
                 (match-string 1 output))))

    (when paste-subtree
      (org-copy-subtree)
      (find-file (expand-file-name path blog-admin-backend-path))
      (goto-char (point-max))
      (insert "\n")
      (org-paste-subtree 1)
      (goto-char (point-min))
      (search-forward "#+END_COMMENT\n\n\n")
      (when (looking-at-p "Write your post here.")
        (org-kill-line)
        (org-next-visible-heading 1)
        (org-kill-line)
        (org-map-entries 'org-promote))
      (save-buffer)
      (kill-buffer)))
  (blog-admin-refresh))

  (blog-admin-backend-define 'nikola
                             `(:scan-posts-func
                               ,#'-scan-posts
                               :read-info-func
                               ,#'-read-info
                               :publish-unpublish-func
                               ,#'-publish-or-unpublish
                               :new-post-func
                               ,#'new-post
                               ))

  ) ;; namespace blog-admin-backend-nikola end here

(provide 'blog-admin-backend-nikola)
;;; blog-admin-backend-nikola.el ends here
