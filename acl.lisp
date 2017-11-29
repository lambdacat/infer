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
  ;; Constraints that reference this cell
  (ref@ (mk-array 3 'constraint))
  (value :unknown))

(defstruct (constraint (:predicate constraint?))
  id
  ;; Cells referred to by this constraint, sorted by ID.
  (cell@ (mk-array 3 'cell))
  update-fn)

(defstruct (network (:predicate network?))
  name ; symbol
  ;; All constraints in the network, indexed by their ID.
  (constraint@ (mk-array 31 'constraint))
  ;; All cells in the network, indexed by their ID.
  (cell@ (mk-array 127 'cell))
  ;; Function to call if two constraints produce an answer for the same cell.
  coincidence-handler
  ;; Function to call if a cell is assigned contradictory values.
  contradiction-handler)

