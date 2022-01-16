(defpackage models
  (:use #:cl)
  (:export
   #:json-kv-pair
   #:key
   #:value
   #:json-array
   #:members
   #:json-object))

(in-package models)

(defclass json-structure () ())

(defclass json-kv-pair (json-structure)
  ((key :type string :accessor key :initarg :key)
   (value :accessor value :initarg :value)))

(defclass json-array (json-structure)
  ((members :type list :accessor members :initarg :members)))

(defclass json-object (json-structure)
  ((members :type list :accessor members :initarg :members)))

(defmethod print-object ((obj json-kv-pair) stream)
  (let ((*standard-output* stream))
    (write-string "#JsonKVPair(")
    (write (key obj))
    (write-string " ")
    (write (value obj))
    (write-string ")")))

(defmethod print-object ((obj json-array) stream)
  (let ((*standard-output* stream))
    (if (members obj)
        (progn (write-string "#JsonArray")
               (write (members obj)))
        (write-string "#JsonArray()"))))

(defmethod print-object ((obj json-object) stream)
  (let ((*standard-output* stream))
    (if (members obj)
        (progn
          (write-string "#JsonObject")
          (write (members obj)))
        (write-string "#JsonObject()"))))
