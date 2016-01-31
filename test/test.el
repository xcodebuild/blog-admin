(defconst sandbox-path "./sandbox")
(defconst blog-path-org-page "./sandbox/org-page-blog")
(defconst blog-path-hexo "./sandbox/hexo-blog")

(if (file-exists-p sandbox-path)
    (delete-directory sandbox-path t))
(setq user-emacs-directory sandbox-path)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(package-refresh-contents)

(package-install 's)
(package-install 'f)
(package-install 'ctable)

(require 'cl) ;; for assert
(require 'ert)

(add-to-list 'load-path "./")
(require 'blog-admin)

;; ready for test
(f-mkdir blog-path-org-page)
(f-mkdir blog-path-hexo)
(toggle-debug-on-error)

;; test org-page

(package-install 'org-page)
(require 'org-page)

(setq blog-admin-backend-type 'org-page)
(setq blog-admin-backend-path blog-path-org-page)
(setq blog-admin-backend-org-page-drafts "_drafts")

(blog-admin-start)

(ert-deftest org-page-new-post-in-drafts-same-dir ()
  (setq blog-admin-backend-new-post-in-drafts t)
  (setq blog-admin-backend-new-post-with-same-name-dir t)
  (blog-admin-backend-org-page-new-post "test.org")
  (should (f-exists? (f-join blog-path-org-page "_drafts/test.org")))
  (should (f-exists? (f-join blog-path-org-page "_drafts/test")))
  (f-delete (f-join blog-path-org-page "_drafts") t)
  )

;; new post not in drafts is not easy to test because ido
;; TODO:fix this

(ert-deftest org-page-new-post-in-drafts-no-same-dir ()
  (setq blog-admin-backend-new-post-in-drafts t)
  (setq blog-admin-backend-new-post-with-same-name-dir nil)
  (blog-admin-backend-org-page-new-post "test.org")
  (should (f-exists? (f-join blog-path-org-page "_drafts/test.org")))
  (should (not (f-exists? (f-join blog-path-org-page "_drafts/test"))))
  (f-delete (f-join blog-path-org-page "_drafts") t)
  )


(ert-run-tests-batch-and-exit)
