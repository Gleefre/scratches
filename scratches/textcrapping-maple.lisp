(defun tree-find (node func)
  (when (xmls:node-p node)
    (append (when (funcall func node)
              (list node))
            (loop for child in (xmls:node-children node)
                  append (tree-find child func)))))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun better (s)
  (replace-all
   (replace-all
    (replace-all
     (replace-all
      (replace-all
       s
       "\\303\\251" "é")
      "\\303\\250" "è")
     "\\303\\240" "â")
    "\\303\\252" "ê")
   "\\303\\264" "ô"))

(format t "~{~a~%~}"
        (loop for nd in (tree-find (xmls:parse (a:read-file-into-string "~/mydata/unige/info/TP5/TP5IntroMaple.mw" :external-format :us-ascii))
                                   (lambda (node)
                                     (member (xmls:xmlrep-tag node)
                                             '("Font" "Text-field")
                                             :test #'equalp)))
              append (loop for ch in (xmls:node-children nd)
                           when (typep ch 'string)
                           collect (better ch))))
