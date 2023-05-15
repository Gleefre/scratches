;;;; Welcome to Portacle, the Portable Common Lisp Environment.
;; For information on Portacle and how to use it, please read
;;   https://portacle.github.io or *portacle-help*
;; To report problems and to get help with issues,  please visit
;;   https://github.com/portacle/portacle/issues
;; Portacle is currently running SLIME , but you can
;;   Switch to SLY
;; You should also configure Portacle with the
;;   First-time setup
;; 
;; You can use this buffer for notes and tinkering with code.

(defun optimized-primes-between (a b)
  (declare (type (unsigned-byte 30) a b)
           (optimize (speed 3)))
  (let ((prime? (make-array b :initial-element 0 :element-type 'bit)))
    (loop for i from 2 to b
          if (zerop (aref prime? (1- i)))
          count (<= a i b)
          and do (loop for j from (* i i) to b by i
                       do (setf (aref prime? (1- j)) 1)))))

(defun primes-between (a b)
  (let ((prime? (make-array b :initial-element t)))
    (loop for i from 2 to b
          if (aref prime? (1- i))
          count (<= a i b)
          and do (loop for j from (* i i) to b by i
                       do (setf (aref prime? (1- j)) nil)))))
