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

(defun robots (&key (robots 10) (height 16) (width 64))
  (labels ((group-rec (n source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (group-rec n rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc)))))
           (group (n source)
             (group-rec n source nil)))
    (loop named main
          with directions = (list (cons 'q (- -1 width)) (cons 'w (- width))
                                  (cons 'e (- 1 width)) (cons 'a -1)
                                  (cons 'd 1) (cons 'z (1- width)) (cons 'x width) (cons 'c (+ 1 width)))
          with player-loses = nil
          for pos = (+ (* (truncate (/ height 2)) width) (truncate (/ width 2)))
          then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
                      (force-output)
                      (let ((new-pos (let* ((c (read))
                                            (d (assoc c directions)))
                                       (cond (d (+ pos (cdr d)))
                                             ((eq 't c) (random (* width height)))
                                             ((eq 'l c) (return-from main 'bye))
                                             (t pos)))))
                        (if (< -1 new-pos (* height width))
                            new-pos
                            (progn
                              (format t "You were teleported by a teleportation wall!.~%")
                              (random (* width height))))))
          for monsters = (loop repeat (max 2 robots)
                               collect (random (* width height)))
          then (loop for mpos in monsters
                     collect (if (> (count mpos monsters) 1)
                                 mpos
                                 (cdar (sort (loop for (k . d) in directions
                                                   for new-mpos = (+ mpos d)
                                                   if (< -1 new-mpos (* width height))
                                                   collect (cons (+ (abs (- (mod new-mpos width) 
                                                                            (mod pos width)))
                                                                    (abs (- (truncate (/ new-mpos height))
                                                                            (truncate (/ pos height)))))
                                                                 new-mpos))
                                             '<
                                             :key #'car))))
          do (format t
                     "~{|~{~A~}|~^~%~}"
                     (group width
                            (append
                             (loop repeat width collect #\-)
                             (loop for p 
                                   below (* width height)
                                   collect (cond ((member p monsters) 
                                                  (cond ((= p pos) (setf player-loses t) #\*)
                                                        ((> (count p monsters) 1) #\#)
                                                        (t #\A)))
                                                 ((= p pos) 
                                                  #\@)
                                                 (t 
                                                  #\ )))
                             (loop repeat width collect #\-))))
          when player-loses return 'player-loses
          when (loop for mpos in monsters
                     always (> (count mpos monsters) 1))
          return 'player-wins)))
