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

(let ((gen (random-state:make-generator 'random-state:mersenne-twister-32 1514)))
  (dotimes (_ 10)
    (print (random-state::mersenne-twister-32-next gen))))

(let ((gen (sb-kernel::seed-random-state 1514)))
  (dotimes (_ 10)
    (print (sb-kernel::random-chunk gen))))

(let ((gen (mt19937:make-random-state)))
  (mt19937:init-random-state 1514 (mt19937::random-state-state gen))
  (dotimes (_ 10)
    (print (mt19937:random-chunk gen))))
