(in-package :cl-user)
(defpackage :cl-openssh-cert.binary
  (:use :cl)
  (:nicknames :openssh-cert.binary :ssh-cert.binary))
(in-package :cl-openssh-cert.binary)

(defgeneric decode (type stream &key)
  (:documentation "Decode a value with the given type and stream" ))
