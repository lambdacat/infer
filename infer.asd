;;;; infer.asd

(asdf:defsystem #:infer
  :description "A toolkit for AI techniques in Common LISP"
  :author "Samantha Payson <scpayson@gmail.com>"
  :license "AGPLv3"
  :serial t
  :components ((:file "package")
	       (:file "util/perf")
	       (:file "util/helper")
	       (:file "acl")
	       (:file "infer")))
