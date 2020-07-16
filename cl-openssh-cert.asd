(defpackage :cl-openssh-cert-system
  (:use :cl :asdf))
(in-package :cl-openssh-cert-system)

(defsystem "cl-openssh-cert"
  :name "cl-openssh-cert"
  :description "Common Lisp library for parsing OpenSSH certificates"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :homepage "https://github.com/dnaeon/cl-openssh-cert"
  :bug-tracker "https://github.com/dnaeon/cl-openssh-cert"
  :source-control "https://github.com/dnaeon/cl-openssh-cert"
  :components ((:module "core"
                :pathname #P"src/"
                :components ((:file "binary")
                             (:file "core" :depends-on ("binary"))))
               (:module "client-package"
                :pathname #P"src/"
                :depends-on ("core")
                :components ((:file "package"))))
  :in-order-to ((test-op (test-op "cl-openssh-cert.test"))))
