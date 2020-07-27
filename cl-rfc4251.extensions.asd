(defpackage :cl-rfc4251-extensions-system
  (:use :cl :asdf))
(in-package :cl-rfc4251-extensions-system)

(defsystem "cl-rfc4251.extensions"
  :name "cl-rfc4251.extensions"
  :long-name "cl-rfc4251.extensions"
  :description "Extensions for cl-rfc4251, providing support for decoding additional data types"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :homepage "https://github.com/dnaeon/cl-rfc4251"
  :bug-tracker "https://github.com/dnaeon/cl-rfc4251"
  :source-control "https://github.com/dnaeon/cl-rfc4251"
  :depends-on (:cl-rfc4251)
  :components ((:module "extensions"
                :pathname #P"src/extensions/"
                :components ((:file "package")
                             (:file "integer" :depends-on ("package"))
                             (:file "ssh-cert" :depends-on ("package")))))
  :in-order-to ((test-op (test-op "cl-rfc4251.test"))))
