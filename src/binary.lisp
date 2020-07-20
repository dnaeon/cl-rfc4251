(in-package :cl-user)
(defpackage :cl-openssh-cert.binary
  (:use :cl)
  (:nicknames :openssh-cert.binary :ssh-cert.binary)
  (:export
   :decode
   :decode-uint-be
   :decode-uint-le))
(in-package :cl-openssh-cert.binary)

(defun decode-uint-be (bytes)
  "Decode a vector of bytes into an unsigned integer, using big-endian byte order"
  (let ((result 0))
    (loop for byte across bytes
          for position from (1- (length bytes)) downto 0
          for bits-to-shift = (* position 8)
          do (setf result (logior result (ash byte bits-to-shift))))
    result))

(defun decode-uint-le (bytes)
  "Decode a vector of bytes into unsigned integer, using litte-endian byte order"
  (decode-uint-be (reverse bytes)))

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

(defmethod decode ((type (eql :uint16-be)) stream &key)
  "Decode 16-bit unsigned integer from the given binary stream"
  (decode-uint-be (decode :raw-bytes stream :length 2)))
