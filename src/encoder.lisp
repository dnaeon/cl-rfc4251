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
(defpackage :cl-rfc4251.encoder
  (:use :cl)
  (:nicknames :rfc4251.encoder)
  (:import-from
   :cl-rfc4251.util
   :encode-uint-be
   :encode-uint-le
   :encode-twos-complement)
  (:export
   :encode))
(in-package :cl-rfc4251.encoder)

(defgeneric encode (type value stream &key)
  (:documentation "Encodes the value of the given type into the binary stream.
Returns the number of bytes that were written to the stream."))

(defmethod encode ((type (eql :byte)) value stream &key)
  "Encodes a raw byte into the given binary stream"
  (let ((size 1))
    (write-byte value stream)
    size))

(defmethod encode ((type (eql :raw-bytes)) bytes-vector stream &key (length (length bytes-vector)))
  (assert (plusp length) (length))
  (loop for i from 0 below length
        for byte across bytes-vector
        do
           (write-byte byte stream)
        finally (return i)))

(defmethod encode ((type (eql :boolean)) value stream &key)
  (let ((byte (if value #x01 #x00)))
    (encode :byte byte stream)))

(defmethod encode ((type (eql :uint16-be)) value stream &key)
  "Encode an unsigned 16-bit integer in big-endian byte order"
  (declare ((unsigned-byte 16) value))
  (encode :raw-bytes (encode-uint-be value :min-size 2) stream))

(defmethod encode ((type (eql :uint16-le)) value stream &key)
  "Encode an unsigned 16-bit integer in little-endian byte order"
  (declare ((unsigned-byte 16) value))
  (encode :raw-bytes (encode-uint-le value :min-size 2) stream))

(defmethod encode ((type (eql :uint16)) value stream &key)
  "Synonym for :uint16-be"
  (encode :uint16-be value stream))

(defmethod encode ((type (eql :uint32-be)) value stream &key)
  "Encode an unsigned 32-bit integer in big-endian byte order"
  (declare ((unsigned-byte 32) value))
  (encode :raw-bytes (encode-uint-be value :min-size 4) stream))

(defmethod encode ((type (eql :uint32-le)) value stream &key)
  "Encode an unsigned 32-bit integer in little-endian byte order"
  (declare ((unsigned-byte 32) value))
  (encode :raw-bytes (encode-uint-le value :min-size 4) stream))

(defmethod encode ((type (eql :uint32)) value stream &key)
  "Synonym for :uint32-be"
  (encode :uint32-be value stream))

(defmethod encode ((type (eql :uint64-be)) value stream &key)
  "Encode an unsigned 64-bit integer in big-endian byte order"
  (declare ((unsigned-byte 64) value))
  (encode :raw-bytes (encode-uint-be value :min-size 8) stream))

(defmethod encode ((type (eql :uint64-le)) value stream &key)
  "Encode an unsigned 64-bit integer in little-endian byte order"
  (declare ((unsigned-byte 64) value))
  (encode :raw-bytes (encode-uint-le value :min-size 8) stream))

(defmethod encode ((type (eql :uint64)) value stream &key)
  "Synonym for :uint64-be"
  (encode :uint64-be value stream))

(defmethod encode ((type (eql :uint-be)) value stream &key)
  "Encode arbitrary-length unsigned integer in big-endian byte order"
  (encode :raw-bytes (encode-uint-be value) stream))

(defmethod encode ((type (eql :uint-le)) value stream &key)
  "Encode arbitrary-length unsigned integer in little-endian byte order"
  (encode :raw-bytes (encode-uint-le value) stream))

(defmethod encode ((type (eql :string)) value stream &key)
  "Encode a string value into the given binary stream"
  (let ((size (length value)))
    (encode :uint32 size stream)
    (loop for char across value do
      (encode :byte (char-code char) stream))
    (+ size 4)))

(defmethod encode ((type (eql :mpint)) value stream &key n-bits mask-bits)
  "Encode a given integer value in two's complement format"
  (declare ((integer) value))

  ;; Propery handle a zero mpint value
  (when (zerop value)
    (return-from encode (encode :uint32-be 0 stream)))

  (let* ((header-size 4) ;; The uint32 header value
         (value-bits (* (ceiling (/ (integer-length value) 8)) 8))
         (data (encode-twos-complement value :n-bits n-bits :mask-bits mask-bits)) ;; The value partition to be written
         (data-length (length data)) ;; The size of the encoded data
         (msb-set-p (logbitp (1- value-bits) value))) ;; Is MSB bit set on the value?
    (if (and (plusp value) msb-set-p)
        ;; Positive integers for which the MSB bit is set should be
        ;; preceeded by the #x00 byte in the encoded data according to RFC 4251.
        (progn
          (encode :uint32 (1+ data-length) stream) ;; Size of the value partition + 1 for the zero byte
          (encode :byte #x00 stream) ;; The preceeding zero byte for positive integers with MSB set
          (encode :raw-bytes data stream) ;; The actual value partition
          (1+ (+ header-size data-length))) ;; 1+ for the extra zero byte
        (progn
          (encode :uint32 data-length stream)
          (encode :raw-bytes data stream)
          (+ header-size data-length)))))
