(ql:quickload 'clinalg)
(defpackage :foo (:use :cl :clinalg))
(in-package :foo)

(make-vector 3)

(let ((m (make-matrix 3 15)))
  (fill-with (lambda (i j) (+ i j)) m))

(let ((m (make-matrix 5 4 :complex-double)))
  (fill-with (lambda (i j) (complex i j))
	     m))

