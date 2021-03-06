(defpackage :cl-rfc4251-system
  (:use :cl :asdf))
(in-package :cl-rfc4251-system)

(defsystem "cl-rfc4251"
  :name "cl-rfc4251"
  :long-name "cl-rfc4251"
  :description "Common Lisp library for encoding and decoding data in RFC 4251 compliant format"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :homepage "https://github.com/dnaeon/cl-rfc4251"
  :bug-tracker "https://github.com/dnaeon/cl-rfc4251"
  :source-control "https://github.com/dnaeon/cl-rfc4251"
  :depends-on (:trivial-gray-streams
               :uiop)
  :components ((:module "util"
                :pathname #P"src/"
                :components ((:file "util")))
               (:module "core"
                :pathname #P"src/"
                :depends-on ("util")
                :components ((:file "decoder")
                             (:file "encoder")
                             (:file "stream")))
               (:module "client-package"
                :pathname #P"src/"
                :depends-on ("core")
                :components ((:file "package"))))
  :in-order-to ((test-op (test-op "cl-rfc4251.test"))))
