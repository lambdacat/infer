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

; (defmacro def-constraint (name cell* &body body))

(defstruct (network (:predicate network?))
  ;; All constraints in the network, indexed by their ID.
  (constraint@ (make-array 31
			   :fill-pointer 0))
  ;; All cells in the network, indexed by their ID.
  (cell@ (make-array 127
		     :fill-pointer 0
		     :element-type 'cell)))
