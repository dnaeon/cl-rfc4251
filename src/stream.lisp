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
(defpackage :cl-rfc4251.stream
  (:use :cl)
  (:nicknames :rfc4251.stream)
  (:import-from
   :trivial-gray-streams
   :fundamental-binary-input-stream
   :fundamental-binary-output-stream
   :stream-read-byte
   :stream-write-byte)
  (:export
   :binary-input-stream
   :binary-input-stream-data
   :binary-input-stream-index
   :binary-input-stream-end
   :make-binary-input-stream
   :binary-output-stream
   :binary-output-stream-data
   :make-binary-output-stream))
(in-package :cl-rfc4251.stream)

(defclass binary-input-stream (fundamental-binary-input-stream)
  ((data
    :initarg :data
    :initform (error "Must provide vector data")
    :reader binary-input-stream-data
    :documentation "A vector providing the underlying data")
   (index
    :initarg :index
    :initform 0
    :reader binary-input-stream-index
    :documentation "Current index position in the data")
   (end
    :initarg :end
    :reader binary-input-stream-end
    :documentation "End marker up to which bytes are to be read"))
  (:documentation "Binary input stream class using a vector as the underlying data"))

(defun make-binary-input-stream (data &key (start 0) (end (length data)))
  "Creates a new instance of BINARY-INPUT-STREAM class"
  (assert (<= 0 start (length data)) (start))
  (assert (<= 0 end (length data)) (end))
  (make-instance 'binary-input-stream
                 :data data
                 :index start
                 :end end))

(defmethod stream-read-byte ((stream binary-input-stream))
  "Reads a byte from the binary stream"
  (with-slots (data index end) stream
    (if (>= index end)
        :eof
        (prog1 (aref data index)
               (incf index)))))

(defmethod print-object ((object binary-input-stream) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (data index end) object
      (format stream "size: ~d index: ~d end: ~d" (length data) index end))))

(defclass binary-output-stream (fundamental-binary-output-stream)
  ((data
    :initarg :data
    :initform (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
    :accessor binary-output-stream-data
    :documentation "The underlying vector to which data is written"))
  (:documentation "Binary output stream class using a vector for the underlying data"))

(defmethod stream-write-byte ((stream binary-output-stream) value)
  "Writes a byte to the given binary stream"
  (with-slots (data) stream
    (vector-push-extend value data))
  value)

(defmethod print-object ((object binary-output-stream) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (data) object
      (format stream "size: ~d" (length data)))))

(defun make-binary-output-stream ()
  "Creates a new instance of BINARY-OUTPUT-STREAM"
  (make-instance 'binary-output-stream))
