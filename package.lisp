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
  (:export #:mapcart
	   #:queue
	   #:queue?
	   #:enqueue
	   #:queue-push
	   #:queue-pop
	   #:zero?
	   #:nil?
	   #:cons?
	   #:atom?
	   #:list?
	   #:length-is?
	   #:show
	   #:take
	   #:drop
	   #:rlambda
	   #:rlet
	   #:any
	   #:fold
	   #:vmapcar
	   #:vmapcan
	   #:vmaplist
	   #:vmapcon
	   #:with-gensyms
	   #:inline-expand))

(defpackage #:acl
  (:use #:cl #:helper))

(defpackage #:infer
  (:use #:cl #:helper))
