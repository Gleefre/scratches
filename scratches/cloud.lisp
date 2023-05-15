(defparameter *sun* (list 13 19 16))

(defun on (&rest pins)
  (loop for pin in pins
        collect (setf (gpio:value pin) t)))

(defun off (&rest pins)
  (loop for pin in pins
        collect (setf (gpio:value pin) nil)))

(apply #'off *sun*)

(defun wink (&rest pins)
  (apply #'on pins)
  (sleep 0.15)
  (apply #'off pins)
  (sleep 0.1))

(defun wink-n (n &rest pins)
  (loop repeat n
        do (apply #'wink pins)))

(defun wink-sun (&optional (sun *sun*))
  (loop for pin in sun
        do (sleep 0.1)
        do (on pin))
  (loop for pin in sun
        do (off pin)
        do (sleep 0.1)))

(defun up ()
  (wink-sun (reverse *sun*)))

(defun down ()
  (wink-sun *sun*))

(defparameter *cloud* (list 14 15 17 18 24 10 26 12 21))

(defun split-cloud (cloud)
  (list (subseq cloud 0 1)
        (subseq cloud 1 3)
        (subseq cloud 3 6)
        (subseq cloud 6 8)
        (subseq cloud 8 9)))

(defun in-out-seq (k list)
  (let ((on (mapcar (lambda (x) (list ':on x)) list))
        (off (mapcar (lambda (x) (list ':off x)) list)))
    (let (result)
      (loop repeat (1- k)
            while on
            do (push (pop on) result))
      (loop while on
            do (push (pop on) result)
            do (push (pop off) result))
      (loop while off
            do (push (pop off) result))
      (reverse result))))

(defun do-seq (seq)
  (loop for (dir pins) in seq
        do (apply (ecase dir ((:on) #'on) ((:off) #'off))
                  pins)
        do (sleep 0.05)))

(defun wink-cloud (&optional (cloud *cloud*))
  (do-seq (in-out-seq 3 (split-cloud cloud))))

(defparameter *wave*
  '((21) (12 26 16) (10 24 18 19) (17 15 13) (14)))
