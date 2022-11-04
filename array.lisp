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
   ;; Complex types
   :complex-float
   :complex-double
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
   ;; Slices
   :copy
   :vector-slice
   :matrix-row
   :matrix-col
   :matrix-slice
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

(defun complex-type? (type)
  (member type '(:complex-float :complex-double)))

(defun real-type (type)
  (case type
    (:complex-float :float)
    (:complex-double :double)
    (t type)))

(defun data-n (type n)
  "Number of elements needed to allocate n numbers of the given type."
  (if (complex-type? type)
      (* 2 n)
      n))

(defun cast-to-cffi-type (x cffi-type)
  "Cast x into a Lisp type that can work with CFFI's type system."
  (let ((type (cdr (assoc cffi-type *cffi-lisp-types*))))
    (cond (type (coerce x type))
	  ((complex-type? cffi-type) (error "Complex type"))
	  (t x))))

(defun foreign-calloc (type n)
  "Returns a garbage-collectable pointer of n elements
  of given type initialized to 0."
  (let ((p (foreign-alloc (real-type type)
			  :count (if (complex-type? type)
				     (* 2 n)
				     n)
			  :initial-element
			  (cast-to-cffi-type 0 (real-type type)))))
    (finalize p (lambda () (foreign-free p)))))

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
  (with-slots (ptr offset type) obj
    (inc-pointer ptr (* offset
			(if (complex-type? type) 2 1)))))

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

(defun memref (ptr type i)
  "Returns p[i] where p's elements have the specified type."
  (macrolet ((ref (i) `(mem-aref ptr (real-type type) ,i)))
    (if (complex-type? type)
	;; Get the real and imaginary parts
	(let ((re (ref (* 2 i)))
	      (im (ref (+ (* 2 i) 1))))
	  (complex re im))
	;; Just a regular real type
	(ref i))))

;; (cast-to-cffi-type 0.0 (real-type :complex-double))

(defun (setf memref) (z ptr type i)
  "Sets p[i] = z."
  (macrolet ((ref (i) `(mem-aref ptr (real-type type) ,i)))
    (if (complex-type? type)
	;; Set complex number
	(let ((typ (real-type type)))
	  (setf (ref (* 2 i))
		(cast-to-cffi-type (realpart z) typ))
	  (setf (ref (+ (* 2 i) 1))
		(cast-to-cffi-type (imagpart z) typ))
	  z) ; return the complex number
	;; Real number
	(setf (ref i) (cast-to-cffi-type z type)))))

(defmethod vref ((v num-vector) i)
  "Returns v[i]."
  (check-vector-in-bounds v i)
  (with-slots (ptr type) v
    (memref ptr type (vector-index v i))))

(defmethod (setf vref) (new-val (v num-vector) i)
  "Sets v[i] = new_val."
  (check-vector-in-bounds v i)
  (with-slots (ptr type) v
    (setf (memref ptr type (vector-index v i))
	  new-val)))

(defmethod mref ((m full-matrix) i j)
  "Returns the element at m[i,j]."
  (check-matrix-in-bounds m i j)
  (with-slots (ptr type) m
    (memref ptr type (matrix-index m i j))))

(defmethod (setf mref) (new-val (m full-matrix) i j)
  "Sets m[i,j] = new_val."
  (check-matrix-in-bounds m i j)
  (with-slots (ptr type) m
    (setf (memref ptr type (matrix-index m i j))
	  new-val)))

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
