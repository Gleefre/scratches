(defun fib-iter (n)
  (declare (optimize speed (safety 0) (debug 0))
           (type fixnum n))
  (let ((stack (make-array (* 2 n) :element-type 'fixnum))
        (stack-pointer 0))
    (declare (type fixnum stack-pointer))
    ;; stack
    (let ((x n) (res 0) (continue 0))
      (declare (type fixnum res x continue))
      ;; registers
      (macrolet ((save (reg)
                   `(setf (aref stack stack-pointer) ,reg
                          stack-pointer (1+ stack-pointer)))
                 (restore (reg)
                   `(setf ,reg (aref stack (decf stack-pointer))))
                 (continue-dispatch ()
                   `(case continue
                      (0 (go done))
                      (1 (go after-fib-x-1))
                      (2 (go after-fib-x-2)))))
        ;; utility macros for dealing with stack
        (block nil
          (tagbody
           fib-rec
             (when (< x 2)
               (setf res x)
               (continue-dispatch))
             ;; prepare call to (fib (- n 1))
             (save continue)
             (save x)
             (setf x (the fixnum (- x 1)))
             (setf continue 1)
             (go fib-rec)
           after-fib-x-1
             ;; prepare call to (fib (- n 2))
             (restore x)
             (setf x (the fixnum (- x 2)))
             (save res)
             (setf continue 2)
             (go fib-rec)
           after-fib-x-2
             ;; add results together and return
             (setf x res)
             (restore res)
             (setf res (the fixnum (+ res x)))
             (restore continue)
             (continue-dispatch)
           done
             (return res)))))))

(defun fib (n)
  (declare (optimize speed (safety 0) (debug 0))
           (type fixnum n))
  (if (< n 2)
      n
      (the fixnum (+ (the fixnum (fib (the fixnum (- n 1))))
                     (the fixnum (fib (the fixnum (- n 2))))))))

(compile 'fib-iter)
(compile 'fib)

(time (fib-iter 40))
(time (fib 40))

#|

|#
