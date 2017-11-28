;;;; package.lisp

(defpackage #:perf
  (:use #:cl)
  (:export #:safely
	   #:debugly
	   #:fastly
	   #:force-fastly
	   #:disasm))

(defpackage #:helper
  (:use #:cl)
  (:export #:mapcart))

(defpackage #:acl
  (:use #:cl #:helper))

(defpackage #:infer
  (:use #:cl #:helper))
