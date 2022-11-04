(in-package :clinalg)

(defmethod copy ((v num-vector))
  "Creates an independent copy of vector v."
  (with-slots (n type) v
    (let ((cp (make-vector n type)))
      (dotimes (i n)
	(setf (vref cp i) (vref v i)))
      cp)))

(defmethod copy ((m full-matrix))
  "Creates an independent copy of matrix m."
  (with-slots (type rows cols) m
    (let ((cp (make-matrix rows cols type)))
      (dotimes (i rows)
	(dotimes (j cols)
	  (setf (mref cp i j) (mref m i j))))
      cp)))

(defun vector-slice (v start end)
  "Returns v[start:end)"
  (check-vector-in-bounds v start)
  (check-vector-in-bounds v (- end 1))
  (let ((start-ind (vector-index v start)))
    (make-instance 'num-vector
		   :n (abs (- end start))
		   :stride (* (stride v)
			      (if (>= end start)
				  1 -1))
		   :type (num-type v)
		   :offset start-ind
		   :ptr (slot-value v 'ptr))))

(defmethod matrix-row ((m full-matrix) i)
  "Returns m[i,:] as a NUM-VECTOR."
  (check-matrix-in-bounds m i 0)
  (with-slots (ptr type cols) m
    (make-instance 'num-vector
		   :n cols
		   :stride 1
		   :type type
		   :offset (matrix-index m i 0)
		   :ptr ptr)))

(defmethod matrix-col ((m full-matrix) j)
  "Returns m[:,j] as a NUM-VECTOR."
  (check-matrix-in-bounds m 0 j)
  (with-slots (ptr type rows ld) m
    (make-instance 'num-vector
		   :n rows
		   :stride ld
		   :type type
		   :offset (matrix-index m 0 j)
		   :ptr ptr)))

(defmethod matrix-slice ((m full-matrix) start-row end-row start-col end-col)
  "Returns m[[start_row:end_row), [start_col:end_col)]."
  (check-matrix-in-bounds m start-row start-col)
  (check-matrix-in-bounds m end-row end-col)
  (assert (>= end-row start-row))
  (assert (>= end-col start-col))
  (with-slots (ptr type ld) m
    (make-instance 'full-matrix
		   :rows (- end-row start-row)
		   :cols (- end-col start-col)
		   :ld ld
		   :type type
		   :offset (matrix-index m start-row start-col)
		   :ptr ptr)))
