(in-package :clinalg)

(defun number->string (x)
  "String indicating the base 10 number 
to 4 decimal places in normal or scientific notation."
  (cond ((integerp x) (format nil "~A" x))
	((complexp x)
	 ;; Complex number
	 (let ((re (realpart x))
	       (im (imagpart x)))
	   (format nil "~A~A~Ai"
		   (number->string re)
		   (if (>= im 0) "+" "-")
		   (number->string (abs im)))))
	((numberp x)
	 ;; Real number
	 (let ((ax (abs x)))
	   (if (and (not (= ax 0))
		    (or (< ax 1e-4) (> ax 1e4)))
	       (format nil "~,4E" x)
	       (format nil "~,4F" x))))
	;; All other data types
	(t (format nil "~A" x))))

(defun spaces (n)
  "Returns a string composed of n spaces."
  (let ((fmt (format nil "~~~A@A" n)))
    (format nil fmt "")))

(defun pad-left (str width)
  "Adds spaces to the left of str to match the
specified width."
  (concatenate 'string
	       (spaces (- width (length str)))
	       str))

(defun max-length (lst)
  "Returns the maximum length of strings in lst."
  (apply #'max (mapcar #'length lst)))

(defun display-indices (n)
  "Lists the indices of rows or columns for pretty-printing."
  (if (> n 10)
      (list 0 1 2 nil (- n 3) (- n 2) (- n 1))
      (loop for i from 0 below n collect i)))

(defun get-printed-matrix-entry (m i j)
  "m[i,j] as a string, or ... if i or j is NIL."
  (if (or (not i) (not j))
      "..."
      (number->string (mref m i j))))

(defun get-column (m col row-indices)
  "m[row_indices,col]"
  (loop for i in row-indices
	collect (get-printed-matrix-entry m i col)))

(defun pretty-print-vector (stream v)
  "Formats the vector v and prints it to stream."
  (let* ((n (size v))
	 (d (display-indices n))
	 (data (loop for i in d
		     collect
		     (if i
			 (number->string (vref v i))
			 "...")))
	 (width (max-length data)))
    (loop for x in data do
      (format stream "~A~%"
	      (pad-left x width)))))
;; (pretty-print-vector t (make-vector 15))

(defun pretty-print-matrix (stream m)
  "Formats the matrix m and prints it to stream."
  (let ((rs (display-indices (rows m)))
	(cs (display-indices (cols m)))
	(last-col (- (cols m) 1)))
    ;; Figure out how wide each column should be
    (let ((col-widths
	    (mapcar (lambda (c)
		      (cons c (max-length (get-column m c rs))))
		    cs)))
      (loop for i in rs do
	(loop for (j . width) in col-widths do
	  ;; Print the column entry
	  (format stream "~A" (pad-left
			  (get-printed-matrix-entry m i j)
			  width))
	  ;; Print column separator or line end
	  (if (and j (= j last-col))
	      (terpri stream)
	      (format stream " ")))))))
;; Example:
;; (pretty-print-matrix t (make-matrix 13 50))

;; Overload the default object printing in Lisp

(defmethod print-object ((obj num-vector) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (format stream "Vector of type ~A (size ~A)~%" (num-type obj) (size obj))
    (pretty-print-vector stream obj)))

(defmethod print-object ((obj full-matrix) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (format stream "Full storage matrix of type ~A (size ~A x ~A)~%" 
       (num-type obj) (rows obj) (cols obj))
    (pretty-print-matrix stream obj)))
