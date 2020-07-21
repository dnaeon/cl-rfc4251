(in-package :cl-user)
(defpackage :cl-openssh-cert.test
  (:use :cl :rove)
  (:nicknames :openssh-cert.test :ssh-cert.test)
  (:import-from
   :openssh-cert.stream
   :binary-input-stream
   :binary-input-stream-data
   :binary-input-stream-index
   :binary-input-stream-end
   :make-binary-input-stream)
  (:import-from
   :openssh-cert.binary
   :decode))
(in-package :cl-openssh-cert.test)

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
      (ok (equalp #(1 2 3 4) (decode :raw-bytes stream :length 4))
          "Decode first slice of four bytes")
      (ok (equalp #(5 6 7 8) (decode :raw-bytes stream :length 4))
          "Decode second slice of four bytes")
      (ok (equalp #(9 10 :eof :eof) (decode :raw-bytes stream :length 4 :eof-error-p nil :eof-value :eof))
          "Decode third slice with eof values")
      (ok (signals (decode :raw-bytes stream :length 1000))
          "Decode out of bounds :length bytes")
      (ok (signals (decode :raw-bytes stream :length -1000))
          "Decode bytes with invalid :length value")))

  (testing "decode boolean"
    (let ((stream (make-binary-input-stream #(0 1 42 128))))
      (ok (equal nil (decode :boolean stream)) "Decode 0 as false")
      (ok (equal t (decode :boolean stream)) "Decode 1 as true")
      (ok (equal t (decode :boolean stream)) "Decode 42 as true")
      (ok (equal t (decode :boolean stream)) "Decode 128 as true")))

  (testing "decode uint16"
    (let* ((data #(#xAB #xCD))
           (stream-be (make-binary-input-stream data))
           (stream-le (make-binary-input-stream data)))
      (ok (= #xABCD (decode :uint16-be stream-be)) "Decode uint16-be")
      (ok (= #xCDAB (decode :uint16-le stream-le)) "Decode uint16-le")))

  (testing "decode uint32"
    (let* ((data #(#x00 #x00 #xAB #xCD))
           (stream-be (make-binary-input-stream data))
           (stream-le (make-binary-input-stream data)))
      (ok (= #x0000ABCD (decode :uint32-be stream-be)) "Decode uint32-be")
      (ok (= #xCDAB0000 (decode :uint32-le stream-le)) "Decode uint32-le")))

  (testing "decode uint64"
    (let* ((data #(#x00 #x00 #x00 #x00 #x00 #x00 #xAB #xCD))
           (stream-be (make-binary-input-stream data))
           (stream-le (make-binary-input-stream data)))
      (ok (= #x000000000000ABCD (decode :uint64-be stream-be)) "Decode uint64-be")
      (ok (= #xCDAB000000000000 (decode :uint64-le stream-le)) "Decode uint64-le")))

  (testing "decode string"
    (let ((stream1 (make-binary-input-stream #(#x00 #x00 #x00 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67)))
          (stream2 (make-binary-input-stream #(#x00 #x00 #x00 #x00)))
          (stream3 (make-binary-input-stream #(#x00 #x00)))) ;; Invalid length, should be uint32
      (ok (string= "testing" (decode :string stream1)) "Decode non-empty string value")
      (ok (string= "" (decode :string stream2)) "Decode empty string value")
      (ok (signals (decode :string stream3)) "Invalid string length")))

  (testing "decode mpint"
    (let ((stream1 (make-binary-input-stream #(#x00 #x00 #x00 #x00)))
          (stream2 (make-binary-input-stream #(#x00 #x00 #x00 #x02 #x00 #x80)))
          (stream3 (make-binary-input-stream #(#x00 #x00 #x00 #x02 #xED #xCC)))
          (stream4 (make-binary-input-stream #(#x00 #x00 #x00 #x05 #xFF #x21 #x52 #x41 #x11))))
      (ok (= #x00 (decode :mpint stream1)) "Decode mpint 0 value")
      (ok (= #x80 (decode :mpint stream2)) "Decode mpint #x80 value")
      (ok (= #x-1234 (decode :mpint stream3)) "Decode mpint #x-1234 value")
      (ok (= #x-DEADBEEF (decode :mpint stream4)) "Decode mpint #x-DEADBEEF value"))))
