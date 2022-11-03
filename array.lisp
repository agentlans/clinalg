;; Copyright 2022 Alan Tseng
;; 
;; This file is part of clinalg.
;; 
;; clinalg is free software: you can redistribute it and/or modify it under the 
;; terms of the GNU Lesser General Public License as published by the Free 
;; Software Foundation, either version 3 of the License, or (at your option) 
;; any later version.
;; 
;; clinalg is distributed in the hope that it will be useful, but WITHOUT ANY 
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
;; FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more 
;; details.
;; 
;; You should have received a copy of the GNU Lesser General Public License 
;; along with clinalg. If not, see <https://www.gnu.org/licenses/>.

(defpackage :clinalg
  (:use :cl
   :cffi :trivial-garbage)
  (:export
   ;; Classes
   :num-vector
   :full-matrix
   ;; Constructors
   :make-vector
   :make-matrix
   ;; Accessors
   :vref
   :mref
   :fill-with
   ;; Properties
   :size
   :stride
   :num-type
   :rows
   :cols
   :ld
   :pointer))
(in-package :clinalg)

(defparameter *cffi-lisp-types*
  '((:float . single-float)
    (:double . double-float)
    (:char . character)
    (:short . integer)
    (:int . integer)
    (:long . integer)
    (:long-long . integer)))

(defun cast-to-cffi-type (x cffi-type)
  "Cast x into a Lisp type that can work with CFFI's type system."
  (let ((type (cdr (assoc cffi-type *cffi-lisp-types*))))
    (if type
	(coerce x type)
	x)))

(defclass num-vector ()
  ((ptr :initarg :ptr)
   (offset :initarg :offset)
   (type :initarg :type
	 :reader num-type)
   (stride :initarg :stride
	   :reader stride)
   (n :initarg :n
      :reader size))
  (:documentation
   "Numeric vector of specified type in C."))

(defclass full-matrix ()
  ((ptr :initarg :ptr)
   (offset :initarg :offset)
   (type :initarg :type
	 :reader num-type)
   (rows :initarg :rows
	 :reader rows)
   (cols :initarg :cols
	 :reader cols)
   (ld :initarg :ld
       :reader ld))
  (:documentation
   "A full storage matrix of specified type in C."))

(defmethod vector-index ((v num-vector) i)
  "Returns the index of v[i]."
  (with-slots (offset stride) v
    (+ offset (* i stride))))

(defmethod matrix-index ((m full-matrix) i j)
  "Returns the index of m[i,j] in row-major order."
  (with-slots (offset ld) m
    (+ offset (* i ld) j)))

(defun foreign-calloc (type n)
  "Returns a garbage-collectable pointer of n elements
  of given type initialized to 0."
  (let ((p (foreign-alloc type :count n
			       :initial-element (cast-to-cffi-type 0 type))))
    (finalize p (lambda () (foreign-free p)))))

(defun make-vector (n &optional (type :double))
  "Returns a vector of length n with the specified type.
Elements are uninitialized."
  (let ((p (foreign-calloc type n)))
    (make-instance
     'num-vector
     :ptr p
     :offset 0
     :type type
     :stride 1
     :n n)))

(defun make-matrix (rows cols &optional (type :double))
  "Returns a matrix with the specified type and dimensions.
Elements are uninitialized."
  (let ((p (foreign-calloc type (* rows cols))))
    (make-instance
     'full-matrix
     :ptr p
     :offset 0
     :type type
     :rows rows
     :cols cols
     :ld cols)))

(defun pointer (obj)
  "Returns pointer to the first element of the array."
  (with-slots (ptr offset) obj
    (inc-pointer ptr offset)))

(defun between? (x bound)
  (and (<= 0 x) (< x bound)))

(defun vector-in-bounds? (v i)
  "Whether i is a valid index for vector v."
  (with-slots (n) v
    (between? i n)))

(defun check-vector-in-bounds (v i)
  (unless (vector-in-bounds? v i)
    (error "Subscript out of bounds.")))

(defun matrix-in-bounds? (m i j)
  "Whether [i,j] is valid index for matrix m."
  (with-slots (rows cols) m
    (and (between? i rows)
	 (between? j cols))))

(defun check-matrix-in-bounds (m i j)
  (unless (matrix-in-bounds? m i j)
    (error "Subscripts out of bounds.")))

(defmethod vref ((v num-vector) i)
  "Returns v[i]."
  (check-vector-in-bounds v i)
  (with-slots (ptr type) v
    (mem-aref ptr type (vector-index v i))))

(defmethod (setf vref) (new-val (v num-vector) i)
  "Sets v[i] = new_val."
  (check-vector-in-bounds v i)
  (with-slots (ptr type) v
    (setf (mem-aref ptr type (vector-index v i))
	  (cast-to-cffi-type new-val type))))

(defmethod mref ((m full-matrix) i j)
  "Returns the element at m[i,j]."
  (check-matrix-in-bounds m i j)
  (with-slots (ptr type) m
    (mem-aref ptr type (matrix-index m i j))))

(defmethod (setf mref) (new-val (m full-matrix) i j)
  "Sets m[i,j] = new_val."
  (check-matrix-in-bounds m i j)
  (with-slots (ptr type) m
    (setf (mem-aref ptr type (matrix-index m i j))
	  (cast-to-cffi-type new-val type))))

;; Sets obj[i] = f(i) for i = 0..size(obj)
(defmethod fill-with (f (obj num-vector))
  (dotimes (i (size obj))
    (setf (vref obj i) (funcall f i)))
  obj)

;; Sets obj[i,j] = f(i,j) for i in [0,rows) and j in [0,cols)
(defmethod fill-with (f (obj full-matrix))
  (dotimes (i (rows obj))
    (dotimes (j (cols obj))
      (setf (mref obj i j) (funcall f i j))))
  obj)
