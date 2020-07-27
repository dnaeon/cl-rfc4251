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
  (let ((size 1)
        (byte (if value #x01 #x00)))
    (write-byte byte stream)
    size))
