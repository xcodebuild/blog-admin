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
(require 'f)
(require 'cl-lib)

(define-namespace blog-admin-backend-nikola-

(defvar posts-dir "posts")
(defvar -posts-dir-cache nil)

(defvar executable nil)
(defvar -draft-posts nil)
(defvar -scheduled-posts nil)
(defvar -published-posts nil)

(defvar config-file "conf.py"
  "filename for nikola configuration file")

;; nikola define

(defun -find-lines (prefix)
  (cl-remove-if-not
   (lambda (x) (s-matches? (format "^%s: " prefix) x))
   (s-lines (buffer-string))))

(defun -find-source (line)
  (nth 1 (s-match "source: \\(.*?\\))" line)))

(defun -file-caching-value (file-attributes)
  "Return the value for a file to be cached.
Currently returns '(filename modification-time)"
  (lambda (file-attributes)
    (list (car file-attributes)
          (nth 5 file-attributes))))

(defun -get-posts-dir-cache-value ()
  "Return list of (file modification-time) values without . & .."
  (mapcar #'-file-caching-value
          (directory-files-and-attributes
           (blog-admin-backend--full-path posts-dir)
           nil
           "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(defun -posts-dir-changed ()
  "Return true if any post has changed"
  (let* ((-posts-dir-cache-new (-get-posts-dir-cache-value))
         (changed (not (equal -posts-dir-cache -posts-dir-cache-new))))
    (setq -posts-dir-cache -posts-dir-cache-new)
    changed))

(defun -scan-posts ()
  "Scan posts of nikola"
  (when (-posts-dir-changed)
    (message "Scanning posts...")
    (let* ((resize-mini-windows nil) ;; Don't show output in mini-buffer
           (command (format
                     "cd %s && %s status -Psd"
                     blog-admin-backend-path
                     blog-admin-backend-nikola-executable))
           (output-buffer-name "*blog-admin-backend-nikola-output*")
           (output (shell-command command output-buffer-name output-buffer-name))
           draft-paths published-paths scheduled-paths)
      (message "Scanning posts... Done!")
      (with-current-buffer output-buffer-name
        (setq draft-paths (mapcar #'-find-source (-find-lines "Draft"))
              published-paths (mapcar #'-find-source (-find-lines "Published"))
              scheduled-paths (mapcar #'-find-source (-find-lines "Scheduled"))
              -draft-posts (mapcar #'blog-admin-backend--full-path draft-paths)
              -published-posts (mapcar #'blog-admin-backend--full-path published-paths)
              -scheduled-posts (mapcar #'blog-admin-backend--full-path scheduled-paths))
        (kill-buffer))))
  (append -draft-posts -published-posts -scheduled-posts))

(defun -is-in-drafts? (post)
  "Return whether is post in drafts"
  (member post -draft-posts))

(defun -is-scheduled? (post)
  "Return whether the post is scheduled"
  (member post -scheduled-posts))

(defun -is-ipynb? (post)
  "Return whether post is an IPython notebook"
  (string-equal (f-ext post) "ipynb"))

(defun -read-nikola-info (post)
  "Read info of any nikola post"
  (with-temp-buffer
    (insert-file-contents post)
    (let ((info nil)
          title date)
      (if (-is-ipynb? post)
          (let* ((nb-json (json-read-file post))
                 (nikola-data (assoc 'nikola (assoc 'metadata nb-json))))
            (setq title (cdr (assoc 'title nikola-data)))
            (setq date (cdr (assoc 'date nikola-data))))

        (setq title
              (s-chop-prefix ".. title: " (car (s-match "^\\.\\. title: .*?\n" (buffer-string)))))
        (setq date
              (s-chop-prefix ".. date: " (car (s-match "^\\.\\. date: .*?\n" (buffer-string))))))

      (setq info (plist-put info :title title))
      (plist-put info :date date))))

(defun -read-info (post)
  "Read info of nikola post"
  (let ((info (-read-nikola-info post)))
    ;; read if publish
    (if (-is-in-drafts? post)
        ;; then
        (plist-put info :publish "NO")
      (if (-is-scheduled? post)
        (plist-put info :publish "TO-BE")
        ;; else
        (plist-put info :publish "YES")))
    ;; format datetime
    (plist-put info :date (blog-admin-backend--format-datetime (plist-get info :date)))))

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


(defun -duplicate ()
  "Duplicate current post"
  (interactive)
  (let* ((post (blog-admin--table-current-file))
         (post-copy (concat (concat (f-no-ext post) "-copy.") (f-ext post)) ))
    (f-copy post post-copy))
  (blog-admin-refresh))

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

(defun build-site ()
  "Build the site."
  (interactive)
  (let ((command (format
                  "cd %s && %s build &"
                  blog-admin-backend-path executable)))
    (-with-venv
     (shell-command command))))

(defun deploy-site ()
  "Deploy the site."
  (interactive)
  (let ((command (format
                  "cd %s && %s deploy &"
                  blog-admin-backend-path executable)))
    (-with-venv
     (shell-command command))))

(defun open-site-config ()
  "Open the config file for nikola"
  (interactive)
  (find-file (blog-admin-backend--full-path config-file)))

(defmacro -with-venv (&rest body)
  "Set exec-path and PATH to use the venv."
  nil
  `(let* ((-venv-dir (f-dirname blog-admin-backend-nikola-executable))
          ;; setup emacs exec-path
          (exec-path (append (list -venv-dir) exec-path))
          (old-path (getenv "PATH"))
          ;; setup the environment for subprocesses
          (path (setenv "PATH" (concat -venv-dir path-separator old-path)))
          result)
     (ignore-errors
       (setq result (progn ,@body)))
     (setenv "PATH" old-path)
     result))

(blog-admin-backend-define 'nikola
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

) ;; namespace blog-admin-backend-nikola end here

(provide 'blog-admin-backend-nikola)
;;; blog-admin-backend-nikola.el ends here
