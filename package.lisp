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
	   #:symbol?
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
	   #:vif
	   #:vwhen
	   #:vwhile
	   #:vmapcar
	   #:vmapcan
	   #:vmaplist
	   #:vmapcon
	   #:vmapc
	   #:with-gensyms
	   #:inline-expand
	   #:amapc
	   #:avmapc))

(defpackage #:acl
  (:use #:cl #:helper))

(defpackage #:infer
  (:use #:cl #:helper))
