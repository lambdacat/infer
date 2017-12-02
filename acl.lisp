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

;;;; ACL.LISP
;;;;
;;;; An implementation of Antecedant Constraint Languages on LISP.
;;;;
;;;; Based roughly on the description in "Building Problem Solvers" by
;;;; Kenneth D. Forbus and Johan de Kleer

(in-package #:acl)

;; Shorthand for MAKE-ARRAY with options commonly used in this code.
(defun mk-array (initial-capacity &optional (type t))
  (make-array initial-capacity
	      :fill-pointer 0
	      :element-type type))

(defstruct (cell (:predicate cell?))
  id
  mark ; For mark/sweep algorithms

  ;; A list of cells whose values are dependent on this one
  (dep@ (mk-array 3 'cell))
  ;; The reason this cell has the value it does.
  informant
  ;; Local name, for printing
  name
  ;; Constraints that reference this cell
  (ref@ (mk-array 3 'constraint))
  (value :UNKNOWN))

(defun unknown? (cell)
  (eq (cell-value cell) :UNKNOWN))

(defun known? (cell)
  (not (unknown? cell)))

(defstruct (constraint (:predicate constraint?))
  id
  ;; Prototype name, for printing
  name
  (queued? nil)
  update-fn)

(defstruct (informant (:predicate informant?))
  ;; The constraint that caused the variable to be set.
  operator
  ;; The cells from which the operator deduced its value.
  input*)

(defun inf-operator (inf)
  (if (eq :USER inf)
      :USER
      (informant-operator inf)))

(Defun inf-input* (inf)
  (if (eq :USER inf)
      nil
      (informant-input* inf)))

(defmethod print-object ((i informant) s)
  (format s "#<Informant ~S~{ ~S~}>"
	  (constraint-name (informant-operator i))
	  (mapcar #'cell-name (informant-input* i))))

(defun default-coincidence-handler (cell value informant)
  (unless (equalp value (cell-value cell))
    (error "Conflicting values for cell ~S.~%   (old) ~S by ~S.~%   (new) ~S by ~S.~%"
	   cell
	   (cell-value cell) (cell-informant cell)
	   value informant)))

(defstruct (network (:predicate network?))
  name ; symbol
  ;; All prototypes for constraints in this network.
  (prototype# (make-hash-table))
  ;; All constraints in the network, indexed by their ID.
  (constraint@ (mk-array 31 'constraint))
  ;; All cells in the network, indexed by their ID.
  (cell@ (mk-array 127 'cell))
  ;; Function to call if two constraints produce an answer for the same cell.
  (coincidence-handler #'default-coincidence-handler)
  ;; Function to call if a cell is assigned contradictory values.
  contradiction-handler
  (running? nil)
  (task-queue (queue)))


(defparameter *NET* (make-network :name 'DEFAULT))

(defun in-network (network)
  (setf *NET* network))

(defmacro with-network (network &body body)
  (with-gensyms (tmp-network)
    `(let ((,tmp-network *NET*))
       (unwind-protect
	    (progn (in-network ,network)
		   ,@body)
	 (in-network ,tmp-network)))))

(defun new-cell (name &optional (*NET* *NET*))
  (let ((cell (make-cell :id (length (network-cell@ *NET*))
			 :name name)))
    (vector-push-extend cell (network-cell@ *NET*))
    cell))

(defun new-constraint (name update-fn)
  (let ((con (make-constraint :id (length (network-cell@ *NET*))
			      :name name
			      :update-fn update-fn)))
    (vector-push-extend con (network-constraint@ *NET*))
    con))

;; Schedule an update for a constraint.
(defun schedule-update (con)
  (unless (constraint-queued? con)    ; Don't queue a constraint twice
    (setf (constraint-queued? con) t)
    (format t "Scheduling update of constraint ~A.~%" (constraint-name con))
    (enqueue (network-task-queue *NET*)
	     (lambda ()
	       (setf (constraint-queued? con) nil)
	       (funcall (constraint-update-fn con) con)))))

;; Set a cell to a given value
(defun set-cell (cell value informant)
  (when (or (unknown? cell)
	    (funcall (network-coincidence-handler *NET*)
		     cell
		     value
		     informant))
    (setf (cell-value cell) value)
    (setf (cell-informant cell) informant)
    (vmapc ((prereq (inf-input* informant)))
      (vector-push-extend cell (cell-dep@ prereq)))
    ;; Schedule an update for every affected constraint.
    (avmapc (i con (cell-ref@ cell))
      (declare (ignore i))
      ;; Don't let a constraint trigger itself
      (unless (eq (inf-operator informant) con)
	(schedule-update con)))))

(defvar *informant* :USER)

;; Schedule a task to set CELL to have VALUE.
(defun set! (cell value)
  (format t "Setting cell ~S to value ~S.~%" (cell-name cell) value)
  (let ((inf *informant*))   ; Grab value of informant, since it might
			     ; not be the same when the task queue is
			     ; run.
    ;; Cell updates take priority, so push them to the front of the
    ;; queue.
    (queue-push (network-task-queue *NET*)
		(lambda ()
		  (set-cell cell value inf)))
    (run-network)))

;; remove the value of a cell, making it unknown
(defun forget-cell (cell)
  (let ((this-pass (list nil))) ; Unique value <=> freshly allocated
    (rlet rec ((cell cell))
      (format t "forgetting cell ~A...~%" (Cell-name cell))
      (unless (eq (cell-mark cell) this-pass)
	(setf (cell-mark cell) this-pass)
	;; make the cell unknown
	(setf (cell-value cell) :unknown)
	(setf (cell-informant cell) nil)
	;; Forget all cells that depend on this cell
	(avmapc (i dep (cell-dep@ cell))
	  (setf (aref (cell-dep@ cell) i) nil) ; Zero the pointer, for GC
	  (rec dep))
	;; Reset the deps list, since they are no longer deps
	(setf (fill-pointer (cell-dep@ cell)) 0)
	;; schedule an update for every affected constraint.
	(avmapc (i con (cell-ref@ cell))
	  (declare (ignore i))
	  (schedule-update con))))))

;; schedule a task to forget the value of cell.
(defun forget! (cell)
  (format t "forgetting cell ~s's value ~s.~%" (cell-name cell) (cell-value cell))
  ;; cell updates take priority, so push them to the front of the
  ;; queue.
  (queue-push (network-task-queue *NET*)
	      (lambda ()
		(forget-cell cell)))
  (run-network))


(defmacro let-cells ((&rest cell*) &body body)
  `(let ,(vmapcar ((cell cell*))
	   (if (symbol? cell)
	       `(,cell (new-cell ',cell))
	       `(,(car cell) (new-cell ',(car cell)))))
     ,@(vmapcan ((cell cell*))
	 (when (list? cell)
	   (list `(set! ,(car cell) ,(cadr cell)))))
     ,@body))

(defun run-network (&optional (*NET* *NET*))
  (unless (network-running? *NET*)
    (unwind-protect
	 (progn
	   (setf (network-running? *NET*) T)
	   (vwhile task (queue-pop (network-task-queue *NET*))
	     (funcall task)))
      (setf (network-running? *NET*) nil))))

(defun prototype (name)
  (vif proto (gethash name (network-prototype# *NET*))
      proto
    (error "no constraint prototype named ~s in network ~s."
	   name
	   (network-name *NET*))))

(defun set-prototype (name proto)
  (setf (gethash name (network-prototype# *NET*)) proto))

(defsetf prototype set-prototype)

(defun connect-constraint (name &rest cell*)
  (format t "connecting constraint ~s!~%"
	  (cons name (mapcar #'cell-name cell*)))
  (let ((con (apply (prototype name) cell*)))
    (vmapc ((cell cell*))
      (vector-push-extend con (cell-ref@ cell)))
    (schedule-update con))
  (run-network))

(defmacro connect! (name result &rest arg*)
  `(connect-constraint ',name ,result ,@arg*))

(defmacro operator! (name &rest arg*)
  (with-gensyms (result)
    `(let-cells (,result)
       (connect-constraint ',name ,result ,@arg*)
       ,result)))

(defun expr->opr (form)
  (if (cons? form)
      `(operator! ,(car form)
		  ,@(mapcar #'expr->opr (cdr form)))
      form))

(defmacro expr! (form)
  (expr->opr form))

(defmacro defcell (&body cell*)
  `(progn ,@(vmapcan ((cell cell*))
	      (if (cons? cell)
		  (list `(defparameter ,(car cell) (new-cell ',(car cell)))
			`(set! ,(car cell) ,(cadr cell)))
		  (list `(defparameter ,cell (new-cell ',cell)))))))

(defmacro def-constraint (name (&rest cell*) &body body)
  `(setf (prototype ',name)
	 (lambda ,cell*
	   (new-constraint
	    ',name
	    (progn
	      ,@body)))))

(defmacro formulae (&body formula*)
  (with-gensyms (constraint)
    `(lambda (,constraint)
       ,@(vmapcar ((formula formula*))
	   `(when (and ,@(vmapcar ((arg (car formula)))
			   `(known? ,arg)))
	      (let ((*informant* (make-informant :operator ,constraint
						 :input* (list ,@(car formula))))
		    ,@(vmapcar ((arg (car formula)))
			`(,arg (cell-value ,arg))))
		,@(cdr formula)))))))

;; Primitive constraints that will be useful all over the place
(defun prelude ()
  ;; R = (+ A B)
  (def-constraint + (r a b)
    (formulae ((a b) (set! r (+ a b)))
	      ((r a) (set! b (- r a)))
	      ((r b) (set! a (- r b)))))

  ;; R = (- A B)
  (def-constraint - (r a b)
    (formulae ((a b) (set! r (- a b)))
	      ((r a) (set! b (- a r)))
	      ((r b) (set! a (+ b r)))))

  ;; A = B
  (def-constraint = (a b)
    (formulae ((a) (set! b a))
	      ((b) (set! a b))))

  ;; R = (* A B)
  (def-constraint * (r a b)
    (formulae ((a b) (set! r (* a b)))
	      ((r a) (set! b (/ r a)))
	      ((r b) (set! a (/ r b)))))

  ;; R = (/ A B)
  (def-constraint / (r a b)
    (formulae ((a b) (set! r (/ a b)))
	      ((r a) (set! b (/ a r)))
	      ((r b) (set! a (* r b)))))

  ;; R = (CONS A B)
  (def-constraint cons (r car cdr)
    (formulae ((car cdr) (set! r (cons car cdr)))
	      ((r) (set! cdr (cdr r))
	       (set! car (car r)))))
  
  (flet ((after-prefix (pfx list)
	   (rlet rec ((p pfx)
		      (l list))
	     (cond ((nil? p) l)
		   ((equal (car p) (car l))
		    (rec (cdr p) (cdr l)))
		   (t (error "No prefix ~A for list ~A." pfx list)))))
	 (before-suffix (list sfx)	; Terrible algorithm =P
	   (rlet rec ((l list)
		      (seen nil))
	     (cond ((nil? l)
		    (unless (nil? sfx)
		      (error "No suffix ~A for list ~A." sfx list)))
		   ((equal sfx l)
		    (nreverse seen))
		   (t (rec (cdr l) (cons (car l) seen)))))))
    ;; R = (APPEND A B)
    (def-constraint append (r a b)
      (formulae ((a b) (set! r (append a b)))
		((r a) (set! b (after-prefix a r)))
		((r b) (set! a (before-suffix r b))))))

  (def-constraint ))
