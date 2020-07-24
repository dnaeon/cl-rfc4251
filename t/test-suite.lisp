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
(defpackage :cl-rfc4251.test
  (:use :cl :rove)
  (:nicknames :rfc4251.test)
  (:import-from
   :cl-rfc4251
   :decode
   :binary-input-stream
   :binary-input-stream-data
   :binary-input-stream-index
   :binary-input-stream-end
   :make-binary-input-stream))
(in-package :cl-rfc4251.test)

(defparameter *binary-input-stream-data*
  #(1 2 3 4 5 6 7 8 9 10)
  "Test data for binary input streams")

(deftest binary-input-stream
  (testing "binary-input-stream with defaults"
    (let ((stream (make-binary-input-stream *binary-input-stream-data*)))
      (ok (equalp *binary-input-stream-data* (binary-input-stream-data stream))
          "Input vector matches binary stream data")
      (ok (= (binary-input-stream-end stream) (length *binary-input-stream-data*))
          "Stream end marker matches length of input vector")
      (ok (= (binary-input-stream-index stream) 0)
          "Stream index marker is at 0")
      (loop for byte across *binary-input-stream-data* do
        (ok (= (read-byte stream) byte)
            (format nil "Byte read from stream is ~d" byte)))
      (ok (equal (read-byte stream nil :eof) :eof)
          "Stream has been exhausted")
      (ok (= (binary-input-stream-index stream) (length *binary-input-stream-data*))
          "Stream index marker is at end")))

  (testing "binary-input-stream with start and end"
    (let ((stream (make-binary-input-stream *binary-input-stream-data* :start 0 :end 4)))
      (ok (equalp (binary-input-stream-data stream) *binary-input-stream-data*)
          "Input vector matches binary stream data")
      (ok (= (binary-input-stream-index stream) 0)
          "Stream index marker is at 0")
      (ok (= (binary-input-stream-end stream) 4)
          "Stream end marker is at 4")
      (ok (= (read-byte stream) 1) "Byte read from stream is 1")
      (ok (= (read-byte stream) 2) "Byte read from stream is 2")
      (ok (= (read-byte stream) 3) "Byte read from stream is 3")
      (ok (= (read-byte stream) 4) "Byte read from stream is 4")
      (ok (equal (read-byte stream nil :eof) :eof)
          "Stream has been exhausted")
      (ok (= (binary-input-stream-index stream) 4)
          "Stream index marker is at 4")))

  (testing "binary-input-stream with invalid start and end"
    (ok (signals (make-binary-input-stream *binary-input-stream-data* :start -10))
        "Negative :start value")
    (ok (signals (make-binary-input-stream *binary-input-stream-data* :start 1000))
        "Out of bounds :start value")
    (ok (signals (make-binary-input-stream *binary-input-stream-data* :end -1000))
        "Negative :end value")
    (ok (signals (make-binary-input-stream *binary-input-stream-data* :end 1000))
        "Out of bounds :end value")
    (ok (signals (make-binary-input-stream *binary-input-stream-data* :start 1000 :end 1000))
        "Invalid :start and :end values")))

(deftest binary-decoder
  (testing "decode raw bytes"
    (let ((stream (make-binary-input-stream *binary-input-stream-data*)))
      (ok (equalp (list #(1 2 3 4) 4)
                  (multiple-value-list (decode :raw-bytes stream :length 4)))
          "Decode first slice of four bytes")
      (ok (equalp (list #(5 6 7 8) 4)
                  (multiple-value-list (decode :raw-bytes stream :length 4)))
          "Decode second slice of four bytes")
      (ok (equalp (list #(9 10 :eof :eof) 4)
                  (multiple-value-list (decode :raw-bytes stream :length 4 :eof-error-p nil :eof-value :eof)))
          "Decode third slice with eof values")
      (ok (signals (decode :raw-bytes stream :length 1000))
          "Decode out of bounds :length bytes")
      (ok (signals (decode :raw-bytes stream :length -1000))
          "Decode bytes with invalid :length value")))

  (testing "decode byte"
    (let ((stream (make-binary-input-stream *binary-input-stream-data*)))
      (loop for index below (length *binary-input-stream-data*) do
        (ok (equal (list (1+ index) 1)
                   (multiple-value-list (decode :byte stream)))
            (format nil "Decode byte at index ~r"  index)))
      (ok (signals (decode :byte stream)) "End of stream")
      (ok (equal (list :eof 1)
                 (multiple-value-list (decode :byte stream :eof-error-p nil :eof-value :eof)))
          "Decode with EOF value")))

  (testing "decode boolean"
    (let ((stream (make-binary-input-stream #(0 1 42 128))))
      (ok (equal (list nil 1)
                 (multiple-value-list (decode :boolean stream)))
          "Decode 0 as false")
      (ok (equal (list t 1)
                 (multiple-value-list (decode :boolean stream)))
          "Decode 1 as true")
      (ok (equal (list t 1)
                 (multiple-value-list (decode :boolean stream)))
          "Decode 42 as true")
      (ok (equal (list t 1)
                 (multiple-value-list (decode :boolean stream)))
          "Decode 128 as true")))

  (testing "decode uint16"
    (let* ((data #(#xAB #xCD))
           (stream-be (make-binary-input-stream data))
           (stream-le (make-binary-input-stream data)))
      (ok (equal (list #xABCD 2)
                 (multiple-value-list (decode :uint16-be stream-be)))
          "Decode uint16-be")
      (ok (equal (list #xCDAB 2)
                 (multiple-value-list (decode :uint16-le stream-le)))
          "Decode uint16-le")))

  (testing "decode uint32"
    (let* ((data #(#x00 #x00 #xAB #xCD))
           (stream1 (make-binary-input-stream data))
           (stream2 (make-binary-input-stream data))
           (stream3 (make-binary-input-stream #(#x29 #xB7 #xF4 #xAA))))
      (ok (equal (list #x0000ABCD 4)
                 (multiple-value-list (decode :uint32-be stream1)))
          "Decode uint32-be")
      (ok (equal (list #xCDAB0000 4)
                 (multiple-value-list (decode :uint32-le stream2)))
          "Decode uint32-le")
      (ok (equal (list #x29B7F4AA 4)
                 (multiple-value-list (decode :uint32-be stream3)))
          "Decode value #x29B7F4AA uint32-be")))

  (testing "decode uint64"
    (let* ((data #(#x00 #x00 #x00 #x00 #x00 #x00 #xAB #xCD))
           (stream-be (make-binary-input-stream data))
           (stream-le (make-binary-input-stream data)))
      (ok (equal (list #x000000000000ABCD 8)
                 (multiple-value-list (decode :uint64-be stream-be)))
          "Decode uint64-be")
      (ok (equal (list #xCDAB000000000000 8)
                 (multiple-value-list (decode :uint64-le stream-le)))
          "Decode uint64-le")))

  (testing "decode string"
    (let ((stream1 (make-binary-input-stream #(#x00 #x00 #x00 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67)))
          (stream2 (make-binary-input-stream #(#x00 #x00 #x00 #x00)))
          (stream3 (make-binary-input-stream #(#x00 #x00)))) ;; Invalid length, should be uint32
      (ok (equal (list "testing" 11) ;; Total number of bytes read should be 4 (uint32) + string length
                 (multiple-value-list (decode :string stream1)))
          "Decode non-empty string value")
      (ok (equal (list "" 4) ;; Total number of bytes read should be 4 (uint32) + 0 (empty string)
                 (multiple-value-list (decode :string stream2)))
          "Decode empty string value")
      (ok (signals (decode :string stream3)) "Invalid string length")))

  (testing "decode mpint"
    (let ((stream1 (make-binary-input-stream #(#x00 #x00 #x00 #x00)))
          (stream2 (make-binary-input-stream #(#x00 #x00 #x00 #x02 #x00 #x80)))
          (stream3 (make-binary-input-stream #(#x00 #x00 #x00 #x02 #xED #xCC)))
          (stream4 (make-binary-input-stream #(#x00 #x00 #x00 #x05 #xFF #x21 #x52 #x41 #x11)))
          (stream5 (make-binary-input-stream #(#x00 #x00 #x00 #x08 #x09 #xA3 #x78 #xF9 #xB2 #xE3 #x32 #xA7)))
          (stream6 (make-binary-input-stream #(#x00 #x00 #x00 #x04))) ;; Missing data partition
          (stream7 (make-binary-input-stream #(#x00 #x00 #x00 #x04 #x01 #x02)))) ;; Incomplete data partition
      (ok (equal (list #x00 4) ;; Total number of bytes read is just 4 (uint32 header)
                 (multiple-value-list (decode :mpint stream1)))
          "Decode mpint 0 value")
      (ok (equal (list #x80 6) ;; Total number of bytes read should be 4 (uint32) + 2 (value partition)
                 (multiple-value-list (decode :mpint stream2)))
          "Decode mpint #x80 value")
      (ok (equal (list #x-1234 6) ;; Total number of bytes read should be 4 (uint32) + 2 (value partition)
                 (multiple-value-list (decode :mpint stream3)))
          "Decode mpint #x-1234 value")
      (ok (equal (list #x-DEADBEEF 9) ;; Total number of bytes should be 4 (uint32) + 5 (value partition)
                 (multiple-value-list (decode :mpint stream4)))
          "Decode mpint #x-DEADBEEF value")
      (ok (equal (list #x9A378F9B2E332A7 12) ;; Total number of bytes read should be 4 (uint32) + 8 (value partition)
                 (multiple-value-list (decode :mpint stream5)))
          "Decode mpint #x9A378F9B2E332A7 value")
      (ok (signals (decode :mpint stream6)) "Missing data partition")
      (ok (signals (decode :mpint stream7)) "Incomplete data partition")))

  (testing "decode name-list"
    (let ((stream1 (make-binary-input-stream #(#x00 #x00 #x00 #x00)))
          (stream2 (make-binary-input-stream #(#x00 #x00 #x00 #x04 #x7A #x6C #x69 #x62)))
          (stream3 (make-binary-input-stream #(#x00 #x00 #x00 #x09 #x7A #x6C #x69 #x62 #x2C #x6E #x6F #x6E #x65))))
      (ok (equal (list nil 4) ;; Total number of bytes should be 4 (uint32 header)
                 (multiple-value-list (decode :name-list stream1)))
          "Decode empty :name-list")
      (ok (equal (list (list "zlib") 8) ;; Total number of bytes read should 4 (uint32) + 4 (value partition)
                 (multiple-value-list (decode :name-list stream2)))
          "Decode :name-list (zlib)")
      (ok (equal (list (list "zlib" "none") 13) ;; Total number of bytes read should 4 (uint32) + 9 (value partition)
                 (multiple-value-list (decode :name-list stream3)))
          "Decode :name-list (zlib none)"))))
