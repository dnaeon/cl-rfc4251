(in-package :cl-user)
(defpackage :cl-openssh-cert.binary
  (:use :cl)
  (:nicknames :openssh-cert.binary :ssh-cert.binary))
(in-package :cl-openssh-cert.binary)

(defgeneric decode (type stream &key)
  (:documentation "Decode a value with the given type and stream" ))

(defmethod decode ((type (eql :raw-bytes)) stream &key (length 1))
  "Read up to the given length of raw bytes from the stream"
  (assert (plusp length) (length))
  (let ((result (make-array length
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0)))
    (loop repeat length do
      (vector-push (read-byte stream) result))
    result))
