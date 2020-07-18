(in-package :cl-user)
(defpackage :cl-openssh-cert.binary
  (:use :cl)
  (:nicknames :openssh-cert.binary :ssh-cert.binary)
  (:export
   :decode))
(in-package :cl-openssh-cert.binary)

(defgeneric decode (type stream &key)
  (:documentation "Decode a value with the given type and stream" ))

(defmethod decode ((type (eql :raw-bytes)) stream &key (length 1) (eof-error-p t) eof-value)
  "Read up to the given length of raw bytes from the stream"
  (assert (plusp length) (length))
  (let ((result (make-array length
                            :fill-pointer 0)))
    (loop repeat length do
      (vector-push (read-byte stream eof-error-p eof-value) result))
    result))

