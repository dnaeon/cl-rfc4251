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
   :encode-int-be
   :encode-int-le
   :decode-int-be
   :decode-int-le
   :twos-complement))
(in-package :cl-rfc4251.util)

(defun encode-int-be (value &key (add-sign t) (min-bytes 1))
  "Encode the given value as bytes in big-endian byte order"
  (assert (plusp min-bytes) (min-bytes))
  (let ((mask #xFF)
        (result (make-array 0
                            :adjustable t
                            :fill-pointer 0
                            :element-type '(unsigned-byte 8)))
        (zero-byte #x00)
        (max-byte #xFF))

    ;; Encode the integer into the result vector
    (loop for byte = (logand value mask)
          until (or (zerop value) (= value -1))
          do
             (vector-push-extend byte result)
             (setf value (ash value -8)))

    ;; Padding
    (loop repeat (- min-bytes (length result)) do
      (vector-push-extend (ldb (byte 8 0) value) result))

    (let ((msb-byte-index (1- (length result)))) ;; We are still in little-endian for now
      ;; Add sign byte, if needed.
      (cond
        ((and add-sign (zerop value) (>= (aref result msb-byte-index) #x80)) ;; Positives
         (vector-push-extend zero-byte result))
        ((and add-sign (= value -1) (< (aref result msb-byte-index) #x80)) ;; Negatives
         (vector-push-extend max-byte result))))

    ;; Represent in big-endian
    (nreverse result)))

(defun decode-int-be (bytes)
  "Decode a vector of bytes into an integer, using big-endian byte order"
  (let ((result 0))
    (loop for byte across bytes
          for position from (1- (length bytes)) downto 0
          for bits-to-shift = (* position 8)
          do (setf result (logior result (ash byte bits-to-shift))))
    result))

(defun encode-int-le (value &key (add-sign t) (min-bytes 1))
  "Convert an integer value to a vector of bytes in little-endian byte order"
  (reverse (encode-int-be value :add-sign add-sign :min-bytes min-bytes)))

(defun decode-int-le (bytes)
  "Decode a vector of bytes into integer, using litte-endian byte order"
  (decode-int-be (reverse bytes)))

(defun twos-complement (n n-bits)
  "Returns the two's complement of the given number"
  (assert (plusp n-bits) (n-bits))
  (let* ((mask (expt 2 (1- n-bits))))
    (+ (- (logand n mask))
       (logand n (lognot mask)))))
