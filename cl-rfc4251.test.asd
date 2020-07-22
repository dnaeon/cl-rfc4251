(defpackage cl-rfc4251-test-system
  (:use :cl :asdf))
(in-package :cl-rfc4251-test-system)

(defsystem "cl-rfc4251.test"
  :name "cl-rfc4251.test"
  :long-name "cl-rfc4251.test"
  :description "Test suite for cl-rfc4251"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :homepage "https://github.com/dnaeon/cl-rfc4251"
  :bug-tracker "https://github.com/dnaeon/cl-rfc4251"
  :source-control "https://github.com/dnaeon/cl-rfc4251"
  :depends-on (:cl-rfc4251
               :rove)
  :components ((:module "tests"
                :pathname #P"t/"
                :components ((:file "test-suite"))))
  :perform (test-op (op c) (uiop:symbol-call :rove :run-suite :cl-rfc4251.test)))
