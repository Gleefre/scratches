(defstruct (delay (:constructor make-delay (function))) (function))

(defun delay-call (thing)
  (if (delay-p thing)
      (funcall (delay-function thing))
      thing))

(defmacro delay (&body body)
  `(make-delay (λ () ,@body)))

(define-modify-macro force () delay-call)

(defmacro lazy-cons (head tail)
  `(cons (delay ,head)
         (delay ,tail)))

(defun lazy-head (lazy-seq)
  (force (car lazy-seq)))

(defun lazy-tail (lazy-seq)
  (force (cdr lazy-seq)))

(defun force-tree (lazy-tree &optional max-depth)
  (labels ((force-tree/rec (thing depth)
             (when (and (consp thing)
                        (not (zerop depth)))
               (force-tree/rec (lazy-head thing) (1- depth))
               (force-tree/rec (lazy-tail thing) (1- depth)))
             (values)))
    (force-tree/rec lazy-tree (or max-depth -1)))
  lazy-tree)

(defun force-seq (lazy-seq &optional max-depth)
  (labels ((force-seq/iter (thing depth)
             (when (and (consp thing)
                        (not (zerop depth)))
               (lazy-head thing)
               (force-seq/iter (lazy-tail thing) (1- depth)))))
    (force-seq/iter lazy-seq (or max-depth -1)))
  lazy-seq)

(defun lazy-range-infinite (a step)
  (lazy-cons a (lazy-range-infinite (+ a step) step)))

(defun lazy-range-up (a b step)
  (when (<= a b)
    (lazy-cons a (lazy-range-up (+ a step) b step))))

(defun lazy-range-down (a b step)
  (when (>= a b)
    (lazy-cons a (lazy-range-down (+ a step) b step))))

(defun lazy-range-repeat (a)
  (lazy-cons a (lazy-range-repeat a)))

(defun lazy-range (a b &optional (step 1))
  (cond ((not b) (lazy-range-infinite a step))
        ((plusp step) (lazy-range-up a b step))
        ((minusp step) (lazy-range-down a b step))
        ((zerop step) (lazy-range-repeat a))))

(defun lazy-append (one another)
  (if one
      (lazy-cons (lazy-head one)
                 (lazy-append (lazy-tail one) another))
      another))

(defun lazy-flatten (tree)
  (if (consp tree)
      (lazy-append (lazy-flatten (car tree))
                   (lazy-flatten (cdr tree)))
      (lazy-cons tree nil)))

(defun lazy-map (function lazy-seq &rest lazy-seqs)
  (when (every (λ (l) (and l (consp l)))
               (list* lazy-seq lazy-seqs))
    (lazy-cons (apply function (lazy-head lazy-seq) (mapcar #'lazy-head lazy-seqs))
               (apply #'lazy-map function (lazy-tail lazy-seq) (mapcar #'lazy-tail lazy-seqs)))))

(defun lazy-remove-if (predicate lazy-seq)
  (when lazy-seq
    (if (funcall predicate (lazy-head lazy-seq))
        (lazy-remove-if predicate (lazy-tail lazy-seq))
        (lazy-cons (lazy-head lazy-seq)
                   (lazy-remove-if predicate (lazy-tail lazy-seq))))))

(defun lazy-remove-if-not (predicate lazy-seq)
  (when lazy-seq
    (if (funcall predicate (lazy-head lazy-seq))
        (lazy-cons (lazy-head lazy-seq)
                   (lazy-remove-if-not predicate (lazy-tail lazy-seq)))
        (lazy-remove-if-not predicate (lazy-tail lazy-seq)))))

(defun lazy-remove (item lazy-seq &key (test #'eql))
  (lazy-remove-if (λ (x) (funcall test item x)) lazy-seq))

(defun lazy-reduce (function lazy-seq &optional (initial-value (funcall function)))
  (labels ((lazy-reduce/iter (seq acc)
             (if seq
                 (lazy-reduce/iter (lazy-tail seq)
                                   (funcall function (lazy-head seq) acc))
                 acc)))
    (lazy-reduce/iter lazy-seq initial-value)))

(defun lazy-merge (lazy-lazy)
  (lazy-reduce #'lazy-append lazy-lazy nil))

(defun queen-n/lazy (n)
  (labels ((rec (k)
             (if (= k 1)
                 (lazy-map #'list (lazy-range 1 n))
                 (lazy-merge
                  (lazy-map (λ (conf)
                               (flet ((good (i)
                                        (loop for r from 1
                                              for j in conf
                                              never (= i j)
                                              never (= r (- j i))
                                              never (= r (- i j)))))
                                 (lazy-map (λ (i) (list* i conf))
                                           (lazy-remove-if-not #'good (lazy-range 1 n)))))
                            (rec (1- k)))))))
    (rec n)))

(defun lazy-sieve (seq)
  (let ((p (lazy-head seq)))
    (lazy-cons p (lazy-sieve
                  (lazy-remove-if (λ (x) (zerop (mod x p)))
                                  (lazy-tail seq))))))

(defun lazy-primes ()
  (lazy-sieve (lazy-range 2 nil)))

(defvar *ones*)
(defparameter *ones* (lazy-cons 1 *ones*))

(defvar *integers*)
(defparameter *integers* (lazy-cons 1 (lazy-map #'+ *ones* *integers*)))
