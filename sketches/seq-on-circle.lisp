(s:eval-always
  (ql:quickload :sketch)
  (ql:quickload :computable-reals))

(defpackage #:sketch-user
  (:use #:cl #:sketch)
  (:local-nicknames (#:sc #:stopclock)))

(in-package #:sketch-user)

(defun make-fib-generator ()
  (let ((a 0) (b 1))
    (lambda ()
      (prog1 a
        (psetf a b
               b (+ a b))))))

(defun make-fib-f3d-generator ()
  (let ((g (make-fib-generator)))
    (lambda (&aux (n (funcall g))
                  (s (princ-to-string n)))
      (read-from-string
       (subseq s 0 (min (length s) 3))))))

(defun make-fib-fNd-generator (|n| &optional (base 10) (mode :cr))
  (setf base (rationalize base))
  (let ((g (make-fib-generator)))
    (lambda (&aux (n (funcall g)))
      (case mode
        (:cr
         (cr:rationalize-r
          (cr:floor-r n
                      (cr:expt-r
                       base
                       (let ((x (cr:floor-r
                                 (cr:-r (cr:log-r n base)
                                        (1- |n|)))))
                         (if (minusp (cr:rational-approx-r x 1)) 0 x))))
          10))
        (:real
         (floor n
                (expt
                 base
                 (max 0 (floor (- (log n base)
                                  (1- |n|)))))))
        (:string
         (let* ((*print-base* base)
                (*read-base* base)
                (s (princ-to-string n)))
           (read-from-string
            (subseq s 0 (min (length s) |n|)))))))))

(defun circle-point (n)
  (list (cos n) (sin n)))

(defsketch c-mapper ((n 5)
                     (base 4)
                     (mode :string)
                     (generator (make-fib-fNd-generator n base mode))
                     (lps 60)
                     (dt (/ lps))
                     (clock (sc:make-clock))
                     (count -1)
                     (points ())
                     (+sf+ 400)
                     (resizable t))
  (loop while (> (sc:time clock) dt)
        do (push (circle-point
                  (* 2 pi (funcall generator) (/ (expt base n))))
                 points)
           (decf (sc:time clock) dt)
           (incf count))
  (with-pen (make-pen :stroke +white+)
    (background +black+)
    (with-fit (((* 2 +sf+) (* 2 +sf+) (- +sf+) (- +sf+))
               (width height))
      (circle 0 0 +sf+)
      (loop for ((x y) (x* y*)) on points
            repeat count
            do (line (* +sf+ x) (* +sf+ y) (* +sf+ x*) (* +sf+ y*))))))

(make-instance 'c-mapper)
