(in-package :cl-user)
(defpackage :cl-rfc4251.binary
  (:use :cl)
  (:nicknames :rfc4251.binary)
  (:export
   :decode
   :decode-uint-be
   :decode-uint-le
   :decode-mpint-be))
(in-package :cl-rfc4251.binary)

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

(defun decode-twos-complement (bytes &optional (n-bits (* (length bytes) 8)))
  "Decodes a two's complement value"
  (let ((mask (expt 2 (1- n-bits)))
        (c (decode-uint-be bytes)))
    (+ (- (logand c mask)) (logand c (lognot mask)))))

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

(defmethod decode ((type (eql :boolean)) stream &key)
  "Decode a boolean value from the given binary stream"
  (let* ((value (read-byte stream)))
    (if (zerop value)
        nil
        t)))

(defmethod decode ((type (eql :uint16-be)) stream &key)
  "Decode 16-bit unsigned integer using big-endian byte order"
  (decode-uint-be (decode :raw-bytes stream :length 2)))

(defmethod decode ((type (eql :uint16-le)) stream &key)
  "Decode 16-bit unsigned integer using little-endian byte order"
  (decode-uint-le (decode :raw-bytes stream :length 2)))

(defmethod decode ((type (eql :uint32-be)) stream &key)
  "Decode 32-bit unsigned integer using big-endian byte order"
  (decode-uint-be (decode :raw-bytes stream :length 4)))

(defmethod decode ((type (eql :uint32-le)) stream &key)
  "Decode 32-bit unsigned integer using little-endian byte order"
  (decode-uint-le (decode :raw-bytes stream :length 4)))

(defmethod decode ((type (eql :uint64-be)) stream &key)
  "Decode 64-bit unsigned integer using big-endian byte order"
  (decode-uint-be (decode :raw-bytes stream :length 8)))

(defmethod decode ((type (eql :uint64-le)) stream &key)
  "Decode 64-bit unsigned integer using little-endian byte order"
  (decode-uint-le (decode :raw-bytes stream :length 8)))

(defmethod decode ((type (eql :string)) stream &key)
  "Decode a string value from the given binary stream"
  (let ((length (decode :uint32-be stream))
        (result (make-string-output-stream)))
    (loop repeat length
          for char = (code-char (read-byte stream))
          do (write-char char result))
    (get-output-stream-string result)))

(defmethod decode ((type (eql :mpint)) stream &key)
  "Decode a multiple precision integer in two's complement format"
  (let* ((length (decode :uint32-be stream))
         (bytes (make-array length :fill-pointer 0)))
    (when (zerop length)
      (return-from decode 0))
    (loop repeat length
          do (vector-push (read-byte stream) bytes))
    (decode-twos-complement bytes)))
