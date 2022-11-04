(ql:quickload 'clinalg)
(defpackage :foo (:use :cl :clinalg))
(in-package :foo)

(make-vector 3)

(let ((m (make-matrix 3 15)))
  (fill-with (lambda (i j) (+ i j)) m))

(let ((m (make-matrix 5 4 :complex-double)))
  (fill-with (lambda (i j) (complex i j))
	     m))

;; Slice part of vector from index 4 (inclusive)
;; to 1 (non-inclusive)
(let ((v (make-vector 5 :complex-double)))
  (dotimes (i 5)
    (setf (vref v i) i))
  (vector-slice v 4 1))

;; Example of slicing a row from a matrix
(defparameter *foo* (make-matrix 3 15))
(fill-with (lambda (i j) (+ i j)) *foo*)
(matrix-row *foo* 2)

