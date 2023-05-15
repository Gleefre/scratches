(ql:quickload :alexandria)

(defparameter *colors* '(♡ ♢ ♣ ♠))
(defparameter *values* '(2 3 4 5 6 7 8 9 10 V D R A))
(defparameter *cards* (alexandria:map-product #'cons *colors* *values*))
(defparameter *hands*
  (let ((list nil))
    (flet ((collect (x) (push x list)))
      (alexandria:map-combinations #'collect *cards* :length 5))
    list))

(defun count-hands (predicate &aux (count 0))
  (flet ((count% (hand)
           (when (funcall predicate hand)
             (incf count))))
    (alexandria:map-combinations #'count% *cards* :length 5)
    count))

(defun square (hand)
  (loop for val in *values*
        thereis (= 4 (count val hand :key #'cdr))))

(defun triple (hand)
  (loop for val in *values*
        thereis (<= 3 (count val hand :key #'cdr))))

(defun triple! (hand)
  (loop for val in *values*
        thereis (= 3 (count val hand :key #'cdr))))

(defun all (&rest functions)
  (lambda (hand)
    (loop for func in functions
          always (funcall func hand))))

(defun one (&rest functions)
  (lambda (hand)
    (loop for func in functions
          thereis (funcall func hand))))

(defun sorted-values (hand)
  (sort
   (loop for card in hand collect (cdr card))
   #'< :key (alexandria:rcurry #'position *values*)))

(defun suite (hand)
  (member (sorted-values hand)
          '((2 3 4 5 A)
            (2 3 4 5 6)
            (3 4 5 6 7)
            (4 5 6 7 8)
            (5 6 7 8 9)
            (6 7 8 9 10)
            (7 8 9 10 V)
            (8 9 10 V D)
            (9 10 V D R)
            (10 V D R A))
          :test 'equal))

(defun one-color (hand)
  (loop for card in hand
        always (eq (car card)
                   (caar hand))))

(defun pair (hand)
  (loop for val in *values*
        thereis (<= 2 (count val hand :key #'cdr))))

(defun pair! (hand)
  (= (loop for val in *values*
           count (>= (count val hand :key #'cdr) 1))
     4))

(defun parmi (n k)
  (alexandria:binomial-coefficient n k))

(defun full (hand)
  (equal '(2 3)
         (sort (loop for val in *values*
                     for cnt = (count val hand :key #'cdr)
                     when (plusp cnt) collect cnt)
               #'<)))

(defun test-13 (hand)
  (and (not (member 'A hand :key #'cdr))
       (member 'R hand :key #'cdr)
       (member '♡ hand :key #'car)))
