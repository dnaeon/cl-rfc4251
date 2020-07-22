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
   :make-binary-input-stream))
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
