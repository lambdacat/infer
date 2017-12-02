;;;; Copyright (ↄ) 2017 Samantha Payson
;;;;
;;;; This file is part of INFER.
;;;;
;;;; INFER is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU Affero General Public License as
;;;; published by the Free Software Foundation, either version 3 of
;;;; the License, or (at your option) any later version.
;;;;
;;;; INFER is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public
;;;; License along with INFER.
;;;;
;;;; If not, see <http://www.gnu.org/licenses/>.

;;;; PERF.LISP
;;;;
;;;; Macros for writing high-performance code.
;;;;
;;;; Based on parts of the Macro Efficiency Topics chapter from Let
;;;; Over Lambda by Doug Hoyte.

(in-package #:perf)

(defvar *debug-build* nil)

;; Ensure that the code in this block has safety checks enabled.
(defmacro safely (&body body)
  `(locally (declare (optimize (speed 0) (safety 3)))
     ,@body))

;; Ensure that the code in this block has safety checks enabled, and
;; includes maximum runtime debugging information.
(defmacro debugly (&body body)
  `(locally (declare (optimize (speed 0) (safety 3) (debug 3)))
     ,@body))

;; In non-debug builds, turn safety checks off and optimize for
;; maximum speed within this block.
(defmacro fastly (&body body)
  (if *debug-build*
      `(debugly
	,@body)
      `(locally (declare (optimize (speed 3) (safety 0)))
	 ,@body)))

;; Like FASTLY, but works even in debug builds
(defmacro force-fastly (&body body)
  `(locally (declare (optimize (speed 3) (safety 0)))
     ,@body))

;; Produce a disassembly listing for BODY. ARG* is a list of
;; variables, optionally with type annotations. E.g.
;;
;;    (disasm (x (y fixnum))
;;      (+ x y))
;;
(defmacro disasm (arg* &body body)
  `(disassemble
    (compile nil
	     (lambda ,(mapcar (lambda (arg)
				(if (consp arg)
				    (car arg)
				    arg))
			      arg*)
	       (declare
		,@(mapcan (lambda (arg)
			    (and (consp arg)
				 (list `(type ,(cadr arg) ,(car arg)))))
			  arg*))
	       ,@body))))
