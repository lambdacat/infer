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

;;;; HELPER.LISP
;;;;
;;;; This file contains various LISP helper functions used throughout
;;;; INFER.

(in-package #:helper)

;;; Consistent Predicate Names

(defun zero? (val)
  (zerop val))

(defun nil? (val)
  (null val))

(defun cons? (val)
  (consp val))

(defun atom? (val)
  (atom val))

(defun list? (val)
  (listp val))

;; Quickly test if a list is a certain exact length.
(defun length-is? (n list)
  (cond ((and (zero? n) (nil? list)) T)
	((or (zero? n) (nil? list)) nil)
	(t (length-is? (1- n) (cdr list)))))

;;; List processing functions

;; Return a list of the first N elements of LS.
;;
;; This returns a newly CONS'd list.
(defun take (n ls)
  (if (or (zero? n) (null ls))
      (cons (car ls)
	    (take (1- n)
		  (cdr ls)))))

;; Return the all but the first N elements of LS.
;;
;; This does not CONS a new list structure.
(defun drop (n ls)
  (nthcdr n ls))


;; A lambda form which can call itself recursively by NAME.
;;
;; Can't be called directly like LAMBDA.
(defmacro rlambda (name lambda-list &body body)
  `(labels ((,name ,lambda-list
	      ,@body))
     #',name))

;; A LET-like macro which allows recursive rebinding. RLET is to
;; RLAMBDA as LET is to LAMBDA.
(defmacro rlet (name (&rest let-arg*) &body body)
  `(funcall (rlambda ,name ,(mapcar #'car let-arg*)
	      ,@body)
	    ,@(mapcar #'cadr let-arg*)))

;; MAPCART maps a function over the cartesian product of the elements
;; of LIST*.
(defun mapcart (fn &rest list*)
  (rlet rec ((arg* nil)
	     (list* (reverse list*)))
    (if list*
	(mapcan (lambda (elem)
		  (rec (cons elem arg*)
		       (cdr list*)))
		(car list*))
	(list (apply fn arg*)))))

;; Return true if FN evaluates to a non-NIL value for any element of
;; LIST.
(defun any1 (fn list)
  (when list
    (or (funcall fn (car list))
	(any1 fn (cdr list)))))

;; Return true if FN evaluates to a non-NIL value for any elements of
;; LIST*.
(defun any (fn &rest list*)
  (unless (any1 #'nil? list*)
    (or (apply fn (mapcar #'car list*))
	(apply #'any fn (mapcar #'cdr list*)))))

;; Accumulate a value by repeatedly calling FN on INIT and the
;; elements of LIST*.
(defun fold (fn init &rest list*)
  (rlet rec ((accum init)
	     (list* list*))
    (if (any #'nil? list*)
	accum
	(apply #'fold
	       fn
	       (apply fn accum (mapcar #'car list*))
	       (mapcar #'cdr list*)))))

(defmacro def-vmap (base-fn vname)
  `(defmacro ,vname ((&rest list-spec*) &body body)
     `(,',base-fn (lambda ,(mapcar #'car list-spec*)
		    ,@body)
		  ,@(mapcar #'cadr list-spec*))))

(def-vmap mapcar vmapcar)
(def-vmap mapcan vmapcan)
(def-vmap maplist vmaplist)
(def-vmap mapcon vmapcon)

;;; Macro helpers

;; Bind gensyms to variables
(defmacro with-gensyms ((&rest var*) &body body)
  `(let ,(vmapcar ((v var*))
	   `(,v (gensym ,(symbol-name v))))
     ,@body))

;; Calls to INLINE-EXPAND expand to the result of evaluating BODY.
(defmacro inline-expand (&body body)
  (with-gensyms (expander)
    `(macrolet ((,expander ()
		  ,@body))
       (,expander))))

;; A quick-and-easy debugging macro which prints a form and its value
;; to standard out.
(defmacro show (val)
  `(format t "~A := ~A~%" ',val ,val))


;;; Queues

(defstruct (queue (:predicate queue?))
  (head nil)
  (tail nil))

(defmethod print-object ((q queue) s)
  (format s "#<Queue~{ ~A~}>" (queue-head q)))

(defun queue-empty? (q)
  (declare (type queue q))
  (perf:fastly
    (nil? (queue-head q))))

(defun queue-push (q &rest arg*)
  (declare (type queue q))
  (perf:fastly
    (setf (queue-head q) (append arg* (queue-head q)))
    (if (nil? (queue-tail q))
	(setf (queue-tail q) (last (queue-head q))))
    q))

(defun enqueue (q &rest arg*)
  (declare (type queue q))
  (perf:fastly
    (if (queue-empty? q)
	(setf (queue-head q) (copy-list arg*)
	      (queue-tail q) (last (queue-head q)))
	(setf (cdr (queue-tail q)) (copy-list arg*)))
    q))

(defun queue (&rest arg*)
  (apply #'enqueue (make-queue) arg*))

(defun queue-pop (q)
  (declare (type queue q))
  (perf:fastly
    (unless (queue-empty? q)
      (pop (queue-head q)))))
