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

(in-package :cl-rfc4251.extensions)

(alexandria:define-constant +ssh-rsa-key-kind+
  "ssh-rsa"
  :test #'string=
  :documentation "The key type for an SSH RSA key")

(defclass ssh-public-key ()
  ((kind
    :initarg :kind
    :initform (error "Must supply key kind")
    :reader ssh-public-key-kind
    :documentation "SSH public key kind")
   (comment
    :initarg :comment
    :initform nil
    :reader ssh-public-key-comment
    :documentation "Comment associated with the SSH public key")
   (data
    :initarg :data
    :initform (error "Must supply public key data")
    :reader ssh-public-key-data
    :documentation "The actual public key data"))
  (:documentation "Represents an OpenSSH public key"))

(defmethod decode ((type (eql :rsa-public-key)) stream &key kind comment)
  "Decode an RSA public key from the given stream.
The data to be read from the stream is expected to be the
public key components of an RSA key - the RSA exponent and the
public modulus."
  (let* ((e-data (multiple-value-list (decode :mpint stream)))
         (e (first e-data))
         (e-size (second e-data))
         (n-data (multiple-value-list (decode :mpint stream)))
         (n (first n-data))
         (n-size (second n-data))
         (pk (make-instance 'ironclad:rsa-public-key :e e :n n)))
    (values
     (make-instance 'ssh-public-key
                    :kind kind
                    :comment comment
                    :data pk)
     (+ e-size n-size))))

(defmethod decode ((type (eql :ssh-rsa-public-key)) stream &key comment)
  "Decodes an SSH RSA public key, encoded as per RFC 4253 document.
RFC 4253 encodes an RSA SSH public key by having the key kind
preceeding the actual RSA public key components."
  (let* ((kind-data (multiple-value-list (decode :string stream)))
         (kind (first kind-data))
         (kind-size (second kind-data))
         (pk-data (multiple-value-list (decode :rsa-public-key stream :kind kind :comment comment)))
         (pk (first pk-data))
         (pk-size (second pk-data)))
    (values pk (+ kind-size pk-size))))

(defun ssh-key-file-parts (path)
  "Returns the parts of an OpenSSH public key file"
  (with-open-file (in path)
    (uiop:split-string (read-line in) :separator '(#\Space))))

(defun parse-ssh-public-key-file (path)
  "Parses an OpenSSH public key file from the given path"
  (let* ((parts (ssh-key-file-parts path))
         (kind (first parts))
         (data (second parts))
         (comment (third parts))
         (stream (make-binary-input-stream (binascii:decode-base64 data))))
    (alexandria:switch (kind :test #'equal)
      (+ssh-rsa-key-kind+ (decode :ssh-rsa-public-key stream :comment comment))
      (t (error "Unknown public key kind ~a" kind)))))
