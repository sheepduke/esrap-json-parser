(defpackage config
  (:use #:cl)
  (:export #:*float-precision*))

(in-package config)

(defvar *float-precision* 'double-float
  "The precision of float values. Defaults to 'double-float.")
