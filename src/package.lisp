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
(defpackage :cl-rfc4251
  (:use :cl)
  (:nicknames :rfc4251)
  (:import-from
   :cl-rfc4251.util
   :encode-int-be
   :encode-int-le
   :decode-int-be
   :decode-int-le
   :twos-complement)
  (:import-from
   :cl-rfc4251.decoder
   :decode)
  (:import-from
   :cl-rfc4251.encoder
   :encode)
  (:import-from
   :cl-rfc4251.stream
   :binary-input-stream
   :binary-input-stream-data
   :binary-input-stream-index
   :binary-input-stream-end
   :make-binary-input-stream
   :binary-output-stream
   :binary-output-stream-data
   :make-binary-output-stream
   :get-binary-stream-bytes
   :with-binary-input-stream
   :with-binary-output-stream)
  (:export
   ;; util
   :encode-int-be
   :encode-int-le
   :decode-int-be
   :decode-int-le
   :twos-complement

   ;; encoder
   :encode

   ;; decoder
   :decode

   ;; stream
   :binary-input-stream
   :binary-input-stream-data
   :binary-input-stream-index
   :binary-input-stream-end
   :make-binary-input-stream
   :binary-output-stream
   :binary-output-stream-data
   :make-binary-output-stream
   :get-binary-stream-bytes
   :with-binary-input-stream
   :with-binary-output-stream))
(in-package :cl-rfc4251)
