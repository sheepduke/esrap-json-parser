(defsystem json
  :version "0.1.0"
  :description ""
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:alexandria
               #:esrap
               #:parse-number)
  :serial t
  :components ((:module "src"
                :components ((:file "config")
                             (:file "models")
                             (:file "rules"))))
  :in-order-to ((test-op (test-op :json-tests))))
