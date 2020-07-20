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

  (testing "decode uint16"
    (let* ((data #(#xab #xcd))
           (stream-be (make-binary-input-stream data))
           (stream-le (make-binary-input-stream data)))
      (ok (= #xabcd (decode :uint16-be stream-be)) "Decode uint16-be")
      (ok (= #xcdab (decode :uint16-le stream-le)) "Decode uint16-le"))))
