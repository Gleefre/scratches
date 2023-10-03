;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defun vector-insert (vector index value)
  (vector-push value vector)
  (replace vector vector :start2 index :start1 (1+ index))
  (setf (aref vector index) value))

(defun vector-delete (vector index)
  (replace vector vector :start2 (1+ index) :start1 index)
  (vector-pop vector)
  (values))

(defun test-vector (n &aux (max-num (expt 10 9)))
  (declare (inline vector-push vector-delete)
           (optimize (debug 0) (safety 0) (speed 3))
           (type fixnum n max-num))
  (let ((array (make-array n :fill-pointer 0 :element-type 'fixnum)))
    ;; phase 1
    (time (loop for size below n
                for num fixnum = (random max-num)
                do (loop with i = 0 and j = size
                         while (> j i)
                         for m = (floor (+ i j) 2)
                         do (if (> (aref array m) num)
                                (setf j m)
                                (setf i (1+ m)))
                         finally (vector-insert array i num))))
    ;; phase 2
    (time (loop for size from n downto 1
                for pos = (random size)
                do (vector-delete array pos))))
  (values))

(defun test-list (n &aux (max-num (expt 10 9)))
  (declare (optimize (debug 0) (safety 0) (speed 3))
           (type fixnum n max-num))
  (let ((list (list 'list)))
    ;; phase 1
    (time (loop with tail = list
                repeat n
                for num = (random max-num)
                do (loop initially (when (< num (the fixnum (cadr tail)))
                                     (setf tail list))
                         while (and (not (endp (rest tail)))
                                    (> num (the fixnum (cadr tail))))
                         do (setf tail (cdr tail))
                         finally (push num (cdr tail)))))
    ;; phase 2
    (time (loop with tail = list
                with cached-pos fixnum = 0
                for size from n downto 1
                for pos = (random size)
                do (when (> cached-pos pos)
                     (setf cached-pos 0
                           tail list))
                   (loop repeat (- pos cached-pos)
                         do (setf tail (cdr tail)
                                  cached-pos (1+ cached-pos))
                         finally (pop (cdr tail)))))
    list)
  (values))
