;;;; package.lisp

(defpackage #:helper
  (:use #:cl)
  (:export #:mapcart))

(defpackage #:infer
  (:use #:cl #:helper))
