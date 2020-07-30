;; Copyright (c) 2020 Marin Atanasov Nikolov <dnaeon@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer
;;     in this position and unchanged.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage :cl-rfc4251.util
  (:use :cl)
  (:nicknames :rfc4251.util)
  (:export
   :encode-uint-be
   :encode-uint-le
   :decode-uint-be
   :decode-uint-le
   :twos-complement
   :encode-twos-complement
   :decode-twos-complement
   :mpint
   :mpint-value
   :mpint-bytes
   :trim-mpint-bytes))
(in-package :cl-rfc4251.util)

(defgeneric mpint-bytes (object &key)
  (:documentation "Returns the bytes of a multiple precision integer value"))

(defgeneric mpint-value (object &key)
  (:documentation "Returns the value representing a multiple precision integer from given bytes"))

(defclass mpint ()
  ((bytes
    :initarg :bytes
    :initform (error "Must specify mpint bytes")
    :documentation "Bytes of the mpint value"))
  (:documentation "Class representing a multiple precision integer value"))

(defmethod mpint-value ((object mpint) &key)
  "Returns the mpint value for the given object"
  (with-slots (bytes) object
    (decode-twos-complement bytes)))

(defmethod mpint-value ((object simple-array) &key)
  "Returns an mpint represented by the the given vector of bytes"
  (decode-twos-complement object))

(defmethod mpint-value ((object integer) &key)
  "Returns the mpint value of the given integer"
  object)

(defmethod mpint-bytes ((object mpint) &key n-bits mask-bits)
  "Returns the bytes for the mpint object"
  (declare (ignore n-bits mask-bits))
  (with-slots (bytes) object
    bytes))

(defmethod mpint-bytes ((object integer) &key n-bits mask-bits)
  "Returns the bytes representing an mpint value for the given integer"
  (encode-twos-complement object :n-bits n-bits :mask-bits mask-bits))

(defun encode-uint-be (value &key (min-size 1))
  "Encode an unsigned integer value to a vector of bytes in big-endian byte order.
The resulting vector will contain at least MIN-SIZE bytes."
  (assert (plusp min-size) (min-size))
  (let* ((value-size-in-bytes (ceiling (/ (integer-length value) 8)))
         (vector-size (max min-size value-size-in-bytes))
         (result (make-array vector-size
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (loop for byte-offset from 0 below value-size-in-bytes
          for vector-index from (1- vector-size) downto 0 do
            (setf (elt result vector-index)
                  (ldb (byte 8 (* 8 byte-offset)) value)))
    result))

(defun decode-uint-be (bytes)
  "Decode a vector of bytes into an unsigned integer, using big-endian byte order"
  (let ((result 0))
    (loop for byte across bytes
          for position from (1- (length bytes)) downto 0
          for bits-to-shift = (* position 8)
          do (setf result (logior result (ash byte bits-to-shift))))
    result))

(defun encode-uint-le (value &key (min-size 1))
  "Convert an integer value to a vector of bytes in little-endian byte order.
The resulting vector will contain at least MIN-SIZE bytes"
  (reverse (encode-uint-be value :min-size min-size)))

(defun decode-uint-le (bytes)
  "Decode a vector of bytes into unsigned integer, using litte-endian byte order"
  (decode-uint-be (reverse bytes)))

(defun encode-twos-complement (n &key n-bits mask-bits)
  "Encodes N into two's complement format"
  (when (zerop n)
    (return-from encode-twos-complement (encode-uint-be 0)))

  (let* ((n-bits (or n-bits (integer-length n)))
         (twos-c (twos-complement n n-bits))
         (mask-bits (or mask-bits (* (ceiling (/ n-bits 8)) 8)))
         (mask (expt 2 mask-bits)))
    (if (minusp twos-c)
        (encode-uint-be (+ mask twos-c))
        (encode-uint-be twos-c))))

(defun decode-twos-complement (bytes &key (n-bits (* (length bytes) 8)))
  "Decodes a two's complement encoded value"
  (assert (plusp n-bits) (n-bits))
  (let ((value (decode-uint-be bytes)))
    (twos-complement value n-bits)))

(defun twos-complement (n n-bits)
  "Returns the two's complement of the given number"
  (assert (plusp n-bits) (n-bits))
  (let* ((mask (expt 2 (1- n-bits))))
    (+ (- (logand n mask))
       (logand n (lognot mask)))))

(defun trim-mpint-bytes (bytes)
  "Trim any unnecessary leading #x00 and #xFF bytes according to RFC 4251"
  (loop for byte across bytes
        for i from 0
        while (or (= byte #x00) (= byte #xFF))
        finally (return (subseq bytes (if (zerop i) i (1- i))))))
