(ql:quickload 'alexandria)

(defstruct (anode)
  (edges (make-hash-table :test 'equal))
  (link nil)
  (len 0)
  (end nil))

(defun make-automaton (&optional s)
  (let* ((enter-node (make-anode))
         (last-node enter-node))
    (loop for ch across s
          do (let ((current-node (make-anode :len (1+ (anode-len last-node)))))
               (loop named nested
                     for node-p = last-node then (anode-link node-p)
                     while node-p
                     for edges = (anode-edges node-p)
                     if (not (gethash ch edges))
                     do (setf (gethash ch edges) current-node)
                     else do (let ((node-q (gethash ch edges)))
                               (if (= (1+ (anode-len node-p)) (anode-len node-q))
                                   (setf (anode-link current-node) node-q)
                                   (let ((clone-node (make-anode
                                                      :edges (alexandria:copy-hash-table (anode-edges node-q))
                                                      :link (anode-link node-q)
                                                      :len (1+ (anode-len node-p)))))
                                     (setf (anode-link current-node) clone-node)
                                     (loop for node = node-p then (anode-link node)
                                           while node
                                           while (eq (gethash ch (anode-edges node)) node-q)
                                           do (setf (gethash ch (anode-edges node)) clone-node))))
                               (return-from nested))
                     finally (setf (anode-link current-node) enter-node))
               (setf last-node current-node)))
    (loop for node = last-node then (anode-link node)
          while node
          do (setf (anode-end node) t))
    (list :enter-node enter-node
          :last-node last-node
          :string s)))

(defun traverse (automaton str)
  (let ((enter-node (getf automaton :enter-node)))
    (loop for ch across str
          for node = (gethash ch (anode-edges enter-node)) then (gethash ch (anode-edges node))
          when (not node) return (values nil nil)
          finally (return (values t (anode-end node))))))
