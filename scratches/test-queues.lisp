(ql:quickload '(:pileup :priority-queue #+nil :cl-containers))

(defun test-pileup (&optional (n (expt 10 6)))
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n))
  (let ((heap (pileup:make-heap #'<)))
    (loop for i below n
          do (pileup:heap-insert (logxor i 12345) heap))
    (loop until (pileup:heap-empty-p heap)
          do (pileup:heap-pop heap))))

#+nil
;; something is not working :/
(defun test-other (&optional (n (expt 10 6)))
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n))
  (let ((heap (make-instance 'cl-containers:priority-queue-on-container)))
    (loop for i below n
          do (cl-containers:insert-item heap (logxor i 12345)))
    (loop while (cl-containers:empty-p heap)
          do (cl-containers:delete-first heap))))

(defun test-pqueue (&optional (n (expt 10 6)))
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n))
  (let ((heap (pqueue:make-pqueue #'< :key-type 'fixnum :value-type 'fixnum)))
    (loop for i below n
          do (pqueue:pqueue-push 0 (logxor i 12345) heap))
    (loop until (pqueue:pqueue-empty-p heap)
          do (pqueue:pqueue-pop heap))))
