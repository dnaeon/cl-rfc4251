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
   :make-binary-input-stream))
(in-package :cl-openssh-cert.test)

(defparameter *binary-input-stream-data*
  #(1 2 3 4 5 6 7 8 9 10)
  "Test data for binary input streams")

(deftest binary-input-stream
  (testing "binary-input-stream with defaults"
    (let ((stream (make-binary-input-stream *binary-input-stream-data*)))
      (ok (equalp *binary-input-stream-data* (binary-input-stream-data stream)))
      (ok (= (binary-input-stream-end stream) (length *binary-input-stream-data*)))
      (ok (= (binary-input-stream-index stream) 0))
      (loop for byte across *binary-input-stream-data* do
        (ok (= (read-byte stream) byte)))
      (ok (equal (read-byte stream nil :eof) :eof)) ;; End of stream
      (ok (= (binary-input-stream-index stream) (length *binary-input-stream-data*)))))

  (testing "binary-input-stream with start and end"
    (let ((stream (make-binary-input-stream *binary-input-stream-data* :start 0 :end 4)))
      (ok (equalp (binary-input-stream-data stream) *binary-input-stream-data*))
      (ok (= (binary-input-stream-index stream) 0))
      (ok (= (binary-input-stream-end stream) 4))
      (ok (= (read-byte stream) 1))
      (ok (= (read-byte stream) 2))
      (ok (= (read-byte stream) 3))
      (ok (= (read-byte stream) 4))
      (ok (equal (read-byte stream nil :eof) :eof)) ;; End of stream
      (ok (= (binary-input-stream-index stream) 4))))

  (testing "binary-input-stream with invalid start and end"
    (ok (signals (make-binary-input-stream *binary-input-stream-data* :start -10)))
    (ok (signals (make-binary-input-stream *binary-input-stream-data* :start 1000)))
    (ok (signals (make-binary-input-stream *binary-input-stream-data* :end -1000)))
    (ok (signals (make-binary-input-stream *binary-input-stream-data* :end 1000)))
    (ok (signals (make-binary-input-stream *binary-input-stream-data* :start 1000 :end 1000)))))
