(defconst sandbox-path "./sandbox")
(defconst blog-path-org-page "./sandbox/org-page-blog")
(defconst blog-path-hexo "./sandbox/hexo-blog")

(if (file-exists-p sandbox-path)
    (delete-directory sandbox-path t))
(setq user-emacs-directory sandbox-path)

(toggle-debug-on-error)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(package-refresh-contents)

(package-install 's)
(package-install 'f)
(package-install 'ctable)
(package-install 'names)

(require 'cl) ;; for assert
(require 'ert)

(add-to-list 'load-path "./")
(require 'blog-admin)


;; test org-page
(f-mkdir blog-path-org-page)

(package-install 'org-page)
(require 'org-page)

;; just some startup configuration
(setq blog-admin-backend-type 'org-page)
(setq blog-admin-backend-path blog-path-org-page)
(setq blog-admin-backend-org-page-drafts "_drafts")
(setq blog-admin-backend-new-post-in-drafts t)
(setq blog-admin-backend-new-post-with-same-name-dir t)
(blog-admin-start)

(ert-deftest org-page-new-post-in-drafts-same-dir ()
  (setq blog-admin-backend-type 'org-page)
  (setq blog-admin-backend-path blog-path-org-page)
  (setq blog-admin-backend-org-page-drafts "_drafts")
  (setq blog-admin-backend-new-post-in-drafts t)
  (setq blog-admin-backend-new-post-with-same-name-dir t)
  (blog-admin-refresh)

  (blog-admin-backend-org-page-new-post "test.org")
  (should (f-exists? (f-join blog-path-org-page "_drafts/test.org")))
  (should (f-exists? (f-join blog-path-org-page "_drafts/test")))
  (f-delete (f-join blog-path-org-page "_drafts") t)
  )

;; new post not in drafts is not easy to test because ido
;; TODO:fix this

(ert-deftest org-page-new-post-in-drafts-no-same-dir ()
  (setq blog-admin-backend-type 'org-page)
  (setq blog-admin-backend-path blog-path-org-page)
  (setq blog-admin-backend-org-page-drafts "_drafts")
  (setq blog-admin-backend-new-post-in-drafts t)
  (setq blog-admin-backend-new-post-with-same-name-dir nil)
  (blog-admin-refresh)

  (blog-admin-backend-org-page-new-post "test.org")
  (should (f-exists? (f-join blog-path-org-page "_drafts/test.org")))
  (should (not (f-exists? (f-join blog-path-org-page "_drafts/test"))))
  (f-delete (f-join blog-path-org-page "_drafts") t)
  )

;; (ert-deftest org-page-new-post-in-drafts-same-dir-with-hook ()
;;   (setq blog-admin-backend-type 'org-page)
;;   (setq blog-admin-backend-path blog-path-org-page)
;;   (setq blog-admin-backend-org-page-drafts "_drafts")
;;   (setq blog-admin-backend-new-post-in-drafts t)
;;   (setq blog-admin-backend-new-post-with-same-name-dir t)
;;   (blog-admin-refresh)
;;   (add-hook 'blog-admin-backend-after-new-post-hook
;;             (lambda (path) (setq HOOK-RESULT path)))
;;   (blog-admin-backend-org-page-new-post "test.org")
;;   (should (f-exists? (f-join blog-path-org-page "_drafts/test.org")))
;;   (should (f-exists? (f-join blog-path-org-page "_drafts/test")))
;;   (should (equal HOOK-RESULT (f-join blog-path-org-page "_drafts/test.org")))
;;   (setq HOOK-RESULT nil)
;;   (f-delete (f-join blog-path-org-page "_drafts") t)
;;   )

;; (ert-deftest org-page-new-post-in-drafts-same-dir-with-many-hook ()
;;   (setq blog-admin-backend-type 'org-page)
;;   (setq blog-admin-backend-path blog-path-org-page)
;;   (setq blog-admin-backend-org-page-drafts "_drafts")
;;   (setq blog-admin-backend-new-post-in-drafts t)
;;   (setq blog-admin-backend-new-post-with-same-name-dir t)
;;   (blog-admin-refresh)
;;   (add-hook 'blog-admin-backend-after-new-post-hook
;;             (lambda (path) (setq HOOK-RESULT path)))
;;   (add-hook 'blog-admin-backend-after-new-post-hook
;;             (lambda (path) (setq HOOK-RESULT-SECOND path)))

;;   (blog-admin-backend-org-page-new-post "test.org")
;;   (should (f-exists? (f-join blog-path-org-page "_drafts/test.org")))
;;   (should (f-exists? (f-join blog-path-org-page "_drafts/test")))
;;   (should (equal HOOK-RESULT (f-join blog-path-org-page "_drafts/test.org")))
;;   (should (equal HOOK-RESULT-SECOND (f-join blog-path-org-page "_drafts/test.org")))
;;   (setq HOOK-RESULT nil)
;;   (setq HOOK-RESULT-SECOND nil)
;;   (f-delete (f-join blog-path-org-page "_drafts") t)
;;   )


;; hexo

(f-mkdir blog-path-hexo)
(f-mkdir (f-join blog-path-hexo "source"))
(f-mkdir (f-join blog-path-hexo "source" "_posts"))
(f-mkdir (f-join blog-path-hexo "source" "_drafts"))


(ert-deftest hexo-new-post-in-drafts-same-dir ()
  (setq blog-admin-backend-type 'hexo)
  (setq blog-admin-backend-path blog-path-hexo)
  (setq blog-admin-backend-new-post-in-drafts t)
  (setq blog-admin-backend-new-post-with-same-name-dir t)
  (blog-admin-refresh)

  (blog-admin-backend-hexo-new-post "test.org")
  (should (f-exists? (f-join blog-path-hexo "source/_drafts/test.org")))
  (should (f-exists? (f-join blog-path-hexo "source/_drafts/test")))
  (f-delete (f-join blog-path-hexo "source/_drafts/test.org"))
  (f-delete (f-join blog-path-hexo "source/_drafts/test"))
  )

(ert-deftest hexo-new-post-in-drafts-no-same-dir ()
  (setq blog-admin-backend-type 'hexo)
  (setq blog-admin-backend-path blog-path-hexo)
  (setq blog-admin-backend-new-post-in-drafts t)
  (setq blog-admin-backend-new-post-with-same-name-dir nil)
  (blog-admin-refresh)

  (blog-admin-backend-hexo-new-post "test.org")
  (should (f-exists? (f-join blog-path-hexo "source/_drafts/test.org")))
  (should (not (f-exists? (f-join blog-path-hexo "source/_drafts/test"))))
  (f-delete (f-join blog-path-hexo "source/_drafts/test.org") t)
  )


;; begin test
(ert-run-tests-batch-and-exit)
