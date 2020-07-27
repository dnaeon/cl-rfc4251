(in-package :cl-user)
(defpackage :cl-rfc4251.extensions
  (:use :cl)
  (:nicknames :rfc4251.extensions)
  (:import-from
   :ironclad)
  (:import-from
   :cl-rfc4251.decoder
   :decode
   :decode-uint-be
   :decode-uint-le)
  (:import-from
   :cl-rfc4251.stream
   :make-binary-input-stream)
  (:export
   ;; ssh-public-key
   :ssh-public-key-kind
   :ssh-public-key-comment
   :ssh-public-key-data
   :parse-ssh-public-key-file))
(in-package :cl-rfc4251.extensions)
