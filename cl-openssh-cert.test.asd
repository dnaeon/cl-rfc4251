(defpackage cl-openssh-cert-test-system
  (:use :cl :asdf))
(in-package :cl-openssh-cert-test-system)

(defsystem "cl-openssh-cert.test"
  :name "cl-openssh-cert.test"
  :long-name "cl-openssh-cert.test"
  :description "Test suite for cl-openssh-cert"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :homepage "https://github.com/dnaeon/cl-openssh-cert"
  :bug-tracker "https://github.com/dnaeon/cl-openssh-cert"
  :source-control "https://github.com/dnaeon/cl-openssh-cert"
  :depends-on (:cl-openssh-cert
               :rove)
  :components ((:module "tests"
                :pathname #P"t/"
                :components ((:file "test-suite"))))
  :perform (test-op (op c) (uiop:symbol-call :rove :run-suite :cl-openssh-cert.test)))
