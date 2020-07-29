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
   :encode
   :binary-input-stream
   :binary-input-stream-data
   :binary-input-stream-index
   :binary-input-stream-end
   :make-binary-input-stream
   :binary-output-stream
   :binary-output-stream-data
   :make-binary-output-stream))
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

(deftest binary-output-stream
  (testing "write to binary-output-stream"
    (let ((stream (make-binary-output-stream)))
      (ok (= 0 (length (binary-output-stream-data stream)))
          "Stream data size is zero")
      (dotimes (i 10)
        (ok (= i (write-byte i stream))
            (format nil "Write byte ~r into stream" i)))
      (ok (= 10 (length (binary-output-stream-data stream)))
          "Stream size is ten")
      (ok (equalp #(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09)
                  (binary-output-stream-data stream))
          "Stream data matches with written bytes"))))

(deftest binary-encoder
  (testing "encode byte"
    (let ((stream (make-binary-output-stream))
          (iterations 10))
      (dotimes (i iterations)
        (ok (= 1 (encode :byte i stream))
            (format nil "Encode byte #x~2,'0x" i)))
      (ok (= iterations (length (binary-output-stream-data stream)))
          (format nil "Binary output stream size is ~d" iterations))))

  (testing "encode raw-bytes"
    (let ((stream1 (make-binary-output-stream))
          (stream2 (make-binary-output-stream))
          (stream3 (make-binary-output-stream))
          (data #(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09)))
      (ok (= (length data) (encode :raw-bytes data stream1))
          "Encode raw bytes into stream1")
      (ok (equalp data (binary-output-stream-data stream1))
          "Encoded data matches with input data")
      (ok (= 5 (encode :raw-bytes data stream2 :length 5))
          "Encode raw-bytes with length 5")
      (ok (equalp #(#x00 #x01 #x02 #x03 #x04)
                  (binary-output-stream-data stream2))
          "Encoded data is a vector of size 5")
      (ok (signals (encode :raw-bytes data stream2 :length 0))
          "Signals on encoding with length 0")
      (ok (signals (encode :raw-bytes data stream2 :length -100))
          "Signals on encoding with negative length")
      (ok (= (length data) (encode :raw-bytes data stream3 :length 1000))
          "Encode raw-bytes with length greater than input data")
      (ok (equalp data (binary-output-stream-data stream3))
          "Encoded raw-bytes matches with input data, while using a greater length")))

  (testing "encode boolean"
    (let ((stream (make-binary-output-stream)))
      (ok (= 1 (encode :boolean t stream))
          "Encode T as boolean")
      (ok (= 1 (encode :boolean nil stream))
          "Encode NIL as boolean")
      (ok (equalp #(#x01 #x00)
                  (binary-output-stream-data stream))
          "Encoded boolean values match with expected data")))

  (testing "encode uint16"
    (let ((value #xABCD)
          (stream1 (make-binary-output-stream))
          (stream2 (make-binary-output-stream)))
      (ok (= 2 (encode :uint16-be value stream1))
          "Encode #xABCD uint16-be -- expect 2 bytes to be written")
      (ok (equalp #(#xAB #xCD)
                  (binary-output-stream-data stream1))
          "Encode #xABCD uint16-be -- expect data to be #(#xAB #xCD)")
      (ok (= 2 (encode :uint16-le value stream2))
          "Encode #xABCD uint16-le -- expect 2 bytes to be written")
      (ok (equalp #(#xCD #xAB)
                  (binary-output-stream-data stream2))
          "Encode #xABCD uint16-le -- expect data to be #(#xCD #xAB)")
      (ok (signals (encode :uint16-be #x29B7F4AA stream1))
          "Expect to signal when using larger than 16-bit value")))

  (testing "encode uint32"
    (let ((stream1 (make-binary-output-stream))
          (stream2 (make-binary-output-stream))
          (stream3 (make-binary-output-stream)))
      (ok (= 4 (encode :uint32-be #x0000ABCD stream1))
          "Encode #x0000ABCD uint32-be -- expect 4 bytes to be written")
      (ok (equalp #(#x00 #x00 #xAB #xCD)
                  (binary-output-stream-data stream1))
          "Encode #x0000ABCD uint32-be -- expect data to be #(#x00 #x00 #xAB #xCD)")
      (ok (= 4 (encode :uint32-le #x0000ABCD stream2))
          "Encode #x0000ABCD uint32-le -- expect 4 bytes to be written")
      (ok (equalp #(#xCD #xAB #x00 #x00)
                  (binary-output-stream-data stream2))
          "Encode #x0000ABCD uint32-le -- expect data to be #(#xCD #xAB #x00 #x00)")
      (ok (= 4 (encode :uint32-be #x29B7F4AA stream3))
          "Encode #x29B7F4AA uint32-le -- expect 4 bytes to be written")
      (ok (equalp #(#x29 #xB7 #xF4 #xAA)
                  (binary-output-stream-data stream3))
          "Encode #x29B7F4AA :uint32-be -- expect data to be #(#x29 #xB7 #xF4 #xAA)")
      (ok (signals (encode :uint32-be #xCDAB000000000000 stream1))
          "Expect to signal when using larger than 32-bit value")))

  (testing "encode uint64"
    (let ((stream1 (make-binary-output-stream))
          (stream2 (make-binary-output-stream)))
      (ok (= 8 (encode :uint64-be #x000000000000ABCD stream1))
          "Encode #x000000000000ABCD :uint64-be -- expect 8 bytes to be written")
      (ok (equalp #(#x00 #x00 #x00 #x00 #x00 #x00 #xAB #xCD)
                  (binary-output-stream-data stream1))
          "Encode #x000000000000ABCD :uint64-be -- expect data to be #(#x00 #x00 #x00 #x00 #x00 #x00 #xAB #xCD)")
      (ok (= 8 (encode :uint64-le #x000000000000ABCD stream2))
          "Encode #x000000000000ABCD :uint64-le -- expect 8 bytes to be written")
      (ok (equalp #(#xCD #xAB #x00 #x00 #x00 #x00 #x00 #x00)
                  (binary-output-stream-data stream2))
          "Encode #x000000000000ABCD :uint64-le -- expect data to be #(#xCD #xAB #x00 #x00 #x00 #x00 #x00 #x00)")
      (ok (signals (encode :uint64-be #xCDAB00000000000000 stream1))
          "Expect to signal when using larger than 64-bit value")))

  (testing "encode string"
    (let ((stream1 (make-binary-output-stream))
          (stream2 (make-binary-output-stream)))
      (ok (= 11 (encode :string "testing" stream1))
          "Encode non-empty string value")
      (ok (equalp #(#x00 #x00 #x00 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67)
                  (binary-output-stream-data stream1))
          "Encoded non-empty string data matches")
      (ok (= 4 (encode :string "" stream2))
          "Encode empty string value")
      (ok (equalp #(#x00 #x00 #x00 #x00)
                  (binary-output-stream-data stream2))
          "Encoded empty string data matches")))

  (testing "encode mpint"
    (let ((stream1 (make-binary-output-stream))
          (stream2 (make-binary-output-stream))
          (stream3 (make-binary-output-stream))
          (stream4 (make-binary-output-stream))
          (stream5 (make-binary-output-stream)))
      (ok (= 4 (encode :mpint 0 stream1))
          "Encode mpint 0 value")
      (ok (equalp #(#x00 #x00 #x00 #x00)
                  (binary-output-stream-data stream1))
          "Encoded mpint 0 value matches with the expected data")
      (ok (= 6 (encode :mpint #x80 stream2)) ;; #x80 has the MSB bit set, so a preceeding zero byte is expected
          "Encode mpint #x80 value")
      (ok (equalp #(#x00 #x00 #x00 #x02 #x00 #x80)
                  (binary-output-stream-data stream2))
          "Encoded mpint #x80 value matches with the expected data")
      (ok (= 6 (encode :mpint #x-1234 stream3))
          "Encode mpint #x-1234 value")
      (ok (equalp #(#x00 #x00 #x00 #x02 #xED #xCC)
                  (binary-output-stream-data stream3))
          "Encoded mpint #x-1234 matches with the expected data")
      (ok (= 9 (encode :mpint #x-DEADBEEF stream4 :mask-bits 40))
          "Encode mpint #x-DEADBEEF value")
      (ok (equalp #(#x00 #x00 #x00 #x05 #xFF #x21 #x52 #x41 #x11)
                  (binary-output-stream-data stream4))
          "Encoded mpint #x-DEADBEEF value matches with the expected data")
      (ok (= 12 (encode :mpint #x9A378F9B2E332A7 stream5 :n-bits 64))
          "Encode mpint #x9A378F9B2E332A7 value")
      (ok (equalp #(#x00 #x00 #x00 #x08 #x09 #xA3 #x78 #xF9 #xB2 #xE3 #x32 #xA7)
                  (binary-output-stream-data stream5))
          "Encoded mpint #x9A378F9B2E332A7 matches with the expected data"))))

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
          "Decode :name-list (zlib none)")))

  (testing "decode ssh-cert-embedded-string-list"
    (let ((stream1 (make-binary-input-stream #(#x00 #x00 #x00 #x14
                                               #x00 #x00 #x00 #x04 #x72 #x6F #x6F #x74
                                               #x00 #x00 #x00 #x08 #x6A #x6F #x68 #x6E #x2E #x64 #x6F #x65)))
          (stream2 (make-binary-input-stream #(#x00 #x00 #x00 #x08
                                               #x00 #x00 #x00 #x04 #x72 #x6F #x6F #x74)))
          (stream3 (make-binary-input-stream #(#x00 #x00 #x00 #x00))))
      (ok (equal (list (list "root" "john.doe") 24) ;; Total number of bytes is 4 (uint32 header) + (4 + length "root") + (4 + length "john.doe")
                 (multiple-value-list (decode :ssh-cert-embedded-string-list stream1)))
          "Decode list of multiple embedded string values")
      (ok (equal (list (list "root") 12) ;; Total number of bytes is 4 (uint32 header) + (4 + length "root")
                 (multiple-value-list (decode :ssh-cert-embedded-string-list stream2)))
          "Decode list of single embedded string value")
      (ok (equal (list nil 4) ;; Total number of bytes is just the uint32 header value
                 (multiple-value-list (decode :ssh-cert-embedded-string-list stream3)))
          "Decode empty list of embedded string values")))

  (testing "decode ssh-cert-options"
    (let ((stream1 (make-binary-input-stream #(#x00 #x00 #x00 #x4C #x00 #x00 #x00 #x0E
                                               #x73 #x6F #x75 #x72 #x63 #x65 #x2D #x61
                                               #x64 #x64 #x72 #x65 #x73 #x73 #x00 #x00
                                               #x00 #x10 #x00 #x00 #x00 #x0C #x31 #x32
                                               #x37 #x2E #x30 #x2E #x30 #x2E #x31 #x2F
                                               #x33 #x32 #x00 #x00 #x00 #x0D #x66 #x6F
                                               #x72 #x63 #x65 #x2D #x63 #x6F #x6D #x6D
                                               #x61 #x6E #x64 #x00 #x00 #x00 #x11 #x00
                                               #x00 #x00 #x0D #x2F #x75 #x73 #x72 #x2F
                                               #x62 #x69 #x6E #x2F #x74 #x72 #x75 #x65)))
          (stream2 (make-binary-input-stream #(#x00 #x00 #x00 #x82 #x00 #x00 #x00 #x15
                                               #x70 #x65 #x72 #x6D #x69 #x74 #x2D #x58
                                               #x31 #x31 #x2D #x66 #x6F #x72 #x77 #x61
                                               #x72 #x64 #x69 #x6E #x67 #x00 #x00 #x00
                                               #x00 #x00 #x00 #x00 #x17 #x70 #x65 #x72
                                               #x6D #x69 #x74 #x2D #x61 #x67 #x65 #x6E
                                               #x74 #x2D #x66 #x6F #x72 #x77 #x61 #x72
                                               #x64 #x69 #x6E #x67 #x00 #x00 #x00 #x00
                                               #x00 #x00 #x00 #x16 #x70 #x65 #x72 #x6D
                                               #x69 #x74 #x2D #x70 #x6F #x72 #x74 #x2D
                                               #x66 #x6F #x72 #x77 #x61 #x72 #x64 #x69
                                               #x6E #x67 #x00 #x00 #x00 #x00 #x00 #x00
                                               #x00 #x0A #x70 #x65 #x72 #x6D #x69 #x74
                                               #x2D #x70 #x74 #x79 #x00 #x00 #x00 #x00
                                               #x00 #x00 #x00 #x0E #x70 #x65 #x72 #x6D
                                               #x69 #x74 #x2D #x75 #x73 #x65 #x72 #x2D
                                               #x72 #x63 #x00 #x00 #x00 #x00)))
          (stream3 (make-binary-input-stream #(#x00 #x00 #x00 #x00))))
      (ok (equal (list '(("source-address" "127.0.0.1/32")
                         ("force-command" "/usr/bin/true"))
                       80) ;; Total number of bytes that were read to produce the options
                 (multiple-value-list (decode :ssh-cert-options stream1)))
          "Decode OpenSSH certificate critical options")
      (ok (equal (list '(("permit-X11-forwarding")
                         ("permit-agent-forwarding")
                         ("permit-port-forwarding")
                         ("permit-pty")
                         ("permit-user-rc"))
                       134) ;; Total number of bytes that were read to produce the extensions
                 (multiple-value-list (decode :ssh-cert-options stream2)))
          "Decode OpenSSH certificate optional extensions")
      (ok (equal (list nil 4)
                 (multiple-value-list (decode :ssh-cert-options stream3)))
          "Decode empty OpenSSH certificate options")))

  (testing "decode ssh-cert-nonce"
    (let ((stream1 (make-binary-input-stream #(#x00 #x00 #x00 #x20
                                               #x0E #xDF #xFF #x9B #x0C #x95 #x80 #xD0
                                               #x5B #x10 #x9C #x4F #x62 #xEE #xC8 #x5E
                                               #xCE #x65 #xA1 #x1C #xEB #x68 #x5E #x51
                                               #xE2 #x6C #x49 #x3C #x13 #xE8 #x15 #x29)))
          (stream2 (make-binary-input-stream #(#x00 #x00 #x00 #x00))))
      (ok (equalp (list #(#x0E #xDF #xFF #x9B #x0C #x95 #x80 #xD0
                          #x5B #x10 #x9C #x4F #x62 #xEE #xC8 #x5E
                          #xCE #x65 #xA1 #x1C #xEB #x68 #x5E #x51
                          #xE2 #x6C #x49 #x3C #x13 #xE8 #x15 #x29)
                        36) ;; Total number of bytes read is 4 (uint32 header) + (length nonce)
                 (multiple-value-list (decode :ssh-cert-nonce stream1)))
          "Decode OpenSSH cert nonce")
      (ok (signals (decode :ssh-cert-nonce stream2))
          "Decode invalid zero-length nonce"))))
