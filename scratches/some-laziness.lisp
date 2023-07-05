(defmacro cons/lazy (head tail)
  `(cons
    (λ () ,head)
    (λ () ,tail)))

(defun head (lazy)
  (funcall (car lazy)))

(defun tail (lazy)
  (funcall (cdr lazy)))

(defun enum/lazy (a b step)
  (if (> a b)
      nil
      (cons/lazy a (enum/lazy (+ a step) b step))))

(defun force/lazy (lazy)
  (if lazy
      (cons (head lazy)
            (force/lazy (tail lazy)))))

(defun append/lazy (one another)
  (if one
      (cons/lazy (head one)
                 (append/lazy (tail one) another))
      another))

(defun traverse/lazy (tree)
  (if (consp tree)
      (append/lazy (traverse/lazy (car tree))
                   (traverse/lazy (cdr tree)))
      (cons/lazy tree nil)))

(defun map/lazy (lazy function)
  (if lazy
      (cons/lazy (funcall function (head lazy))
                 (map/lazy (tail lazy) function))))

(defun filter/lazy (lazy predicate)
  (if lazy
      (if (funcall predicate (head lazy))
          (cons/lazy (head lazy)
                     (filter/lazy (tail lazy) predicate))
          (filter/lazy (tail lazy) predicate))))

(defun reduce/lazy (lazy function initial-value)
  (if lazy
      (funcall function
               (head lazy)
               (reduce/lazy (tail lazy) function initial-value))
      initial-value))

(defun flatten/lazy (lazy-lazy)
  (reduce/lazy lazy-lazy #'append/lazy nil))

(defun queen/lazy (n)
  (labels ((queen/good (i conf)
             (loop for r from 1
                   for j in conf
                   never (= i j)
                   never (= r (- j i))
                   never (= r (- i j))))
           (queen/rec (k)
             (if (= k 1)
                 (map/lazy (enum/lazy 1 n 1)
                           (λ (x) (list x)))
                 (flatten/lazy
                  (map/lazy (queen/rec (1- k))
                            (λ (conf)
                               (map/lazy
                                (filter/lazy
                                 (enum/lazy 1 n 1)
                                 (λ (i)
                                    (queen/good i conf)))
                                (λ (i)
                                   (list* i conf)))))))))
    (queen/rec n)))
