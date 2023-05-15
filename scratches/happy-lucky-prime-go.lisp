(defun digits (n &optional (base 10))
  (do ((digits ()))
      ((zerop n) digits)
    (multiple-value-bind (n/base remainder)
        (floor n base)
      (setf n n/base)
      (push remainder digits))))

(defun happy-next-step (n)
  (loop for dg in (digits n)
        sum (expt dg 2)))

(defun happy? (n &optional (steps 1000) &aux (was-here (make-hash-table)))
  (loop repeat steps
        while (not (gethash n was-here))
        do (psetf n (happy-next-step n)
                  (gethash n was-here) t))
  (loop for k = n then (happy-next-step k)
        while (gethash k was-here)
        minimize k
        do (setf (gethash k was-here) nil)))

(defun prime? (n)
  (and (> n 1)
       (loop for i from 2 to (1- n)
             never (zerop (mod n i)))))

(defun lucky-sieve (N &aux (lucky-array (make-array N :element-type 'boolean :initial-element t)))
  (dotimes (i (1- N))
    (when (and (aref lucky-array i) (> i 0))
      (let ((cnt 0))
        (dotimes (j N)
          (when (aref lucky-array j)
            (incf cnt)
            (when (zerop (mod cnt (1+ i)))
              (setf (aref lucky-array j) nil)))))))
  (loop for i below N
        when (aref lucky-array i)
        collect (1+ i)))

(defun lucky? (n)
  (member n
          '#.(lucky-sieve 10000)))
