#!/usr/bin/env sh

set -e

sbcl --eval '(ql:quickload :cl-rfc4251.test)' \
     --eval '(setf rove:*enable-colors* nil)' \
     --eval '(asdf:test-system :cl-rfc4251.test)' \
     --eval '(quit)'
