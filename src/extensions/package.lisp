(in-package :cl-user)
(defpackage :cl-rfc4251.extensions
  (:use :cl)
  (:nicknames :rfc4251.extensions)
  (:import-from
   :cl-rfc4251.binary
   :decode
   :decode-uint-be
   :decode-uint-le))
(in-package :cl-rfc4251.extensions)
