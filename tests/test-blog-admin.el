(require 'blog-admin)

(describe "equire fine"
          (it "can be require fine"
              (expect blog-admin-mode-buffer
                      :to-equal
                      nil)))
