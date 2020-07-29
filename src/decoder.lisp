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
(defpackage :cl-rfc4251.decoder
  (:use :cl)
  (:nicknames :rfc4251.decoder)
  (:import-from
   :uiop
   :split-string)
  (:import-from
   :cl-rfc4251.util
   :decode-uint-be
   :decode-uint-le
   :decode-twos-complement)
  (:export
   :decode))
(in-package :cl-rfc4251.decoder)

(defgeneric decode (type stream &key)
  (:documentation "Decode a value with the given type from the binary
stream. Returns multiple values -- the decoded value and the number of
bytes that were actually read to produce the value."))

(defmethod decode ((type (eql :raw-bytes)) stream &key (length 1) (eof-error-p t) eof-value)
  "Read up to the given length of raw bytes from the stream"
  (assert (plusp length) (length))
  (let ((result (make-array length :fill-pointer 0))
        (size length))
    (loop repeat length do
      (vector-push (read-byte stream eof-error-p eof-value) result))
    (values result size)))

(defmethod decode ((type (eql :byte)) stream &key (eof-error-p t) eof-value)
  "Decode a single byte (octet) from the given binary stream"
  (let ((size 1))
    (values (read-byte stream eof-error-p eof-value) size)))

(defmethod decode ((type (eql :boolean)) stream &key)
  "Decode a boolean value from the given binary stream"
  (let* ((size 1)
         (byte (read-byte stream))
         (result (if (zerop byte) nil t)))
    (values result size)))

(defmethod decode ((type (eql :uint16-be)) stream &key)
  "Decode 16-bit unsigned integer using big-endian byte order"
  (let ((size 2))
    (values
     (decode-uint-be (decode :raw-bytes stream :length size))
     size)))

(defmethod decode ((type (eql :uint16-le)) stream &key)
  "Decode 16-bit unsigned integer using little-endian byte order"
  (let ((size 2))
    (values
     (decode-uint-le (decode :raw-bytes stream :length size))
     size)))

(defmethod decode ((type (eql :uint16)) stream &key)
  "Synonym for :uint16-be"
  (decode :uint16-be stream))

(defmethod decode ((type (eql :uint32-be)) stream &key)
  "Decode 32-bit unsigned integer using big-endian byte order"
  (let ((size 4))
    (values
     (decode-uint-be (decode :raw-bytes stream :length size))
     size)))

(defmethod decode ((type (eql :uint32-le)) stream &key)
  "Decode 32-bit unsigned integer using little-endian byte order"
  (let ((size 4))
    (values
     (decode-uint-le (decode :raw-bytes stream :length size))
     size)))

(defmethod decode ((type (eql :uint32)) stream &key)
  "Synonym for :uint32-be"
  (decode :uint32-be stream))

(defmethod decode ((type (eql :uint64-be)) stream &key)
  "Decode 64-bit unsigned integer using big-endian byte order"
  (let ((size 8))
    (values
     (decode-uint-be (decode :raw-bytes stream :length size))
     size)))

(defmethod decode ((type (eql :uint64-le)) stream &key)
  "Decode 64-bit unsigned integer using little-endian byte order"
  (let ((size 8))
    (values
     (decode-uint-le (decode :raw-bytes stream :length size))
     size)))

(defmethod decode ((type (eql :uint64)) stream &key)
  "Synonym for :uint64-be"
  (decode :uint64-be stream))

(defmethod decode ((type (eql :string)) stream &key)
  "Decode a string value from the given binary stream"
  (let ((size 4) ;; Size of the uint32 number specifying the string length
        (length (decode :uint32-be stream))
        (result (make-string-output-stream)))
    (loop repeat length
          for char = (code-char (read-byte stream))
          do (write-char char result))
    (values
     (get-output-stream-string result)
     (+ size length))))

(defmethod decode ((type (eql :mpint)) stream &key)
  "Decode a multiple precision integer in two's complement format"
  (let* ((size 4) ;; Size of the uint32 number specifying the mpint length
         (length (decode :uint32-be stream))
         (bytes (make-array length :fill-pointer 0)))
    (when (zerop length)
      (return-from decode (values 0 size)))
    (loop repeat length
          do (vector-push (read-byte stream) bytes))
    (values
     (decode-twos-complement bytes)
     (+ size length))))

(defmethod decode ((type (eql :name-list)) stream &key)
  "Decode a comma-separated list of names from the given binary stream"
  (multiple-value-bind (value size) (decode :string stream)
    (values
     (split-string value :separator (list #\Comma))
     size)))
