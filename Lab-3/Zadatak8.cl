(defun breadth-first8(graph traversed goal visited)
  (cond
   ((null traversed) NIL)
   ((equal (car traversed) goal) (list goal))
   (:else (let* 
              ((new-visited (append visited (list (car traversed))))
               (children (add-children8 graph (car traversed)
                                       (append (cdr traversed) new-visited)))
               (new-traversed (append (cdr traversed) children))
               (found-path (breadth-first8 graph new-traversed goal new-visited)))
            (cond
             ((null found-path) NIL)
             ((member (car found-path) children) (cons (car traversed) found-path))
             (:else found-path))))
   )
  )

;;; Returns unvisited nodes from the children of a node
(defun add-children8(graph node visited)
  (cond
   ((null graph) NIL)
   ((equal (caar graph) node) (new-nodes8 (cadar graph) visited))
   (:else (add-children8 (cdr graph) node visited))
   )
  )

;;; Returns new, unvisited nodes from the children of a node
(defun new-nodes8(children visited)
  (cond
   ((null children) NIL)
   ((member (car children) visited) (new-nodes8 (cdr children) visited))
   (:else (cons (car children) (new-nodes8 (cdr children) visited)))
   )
  )


(defun breadth-first-conc8(graph traversed1 traversed2 visited1 visited2)
  (cond
   ((null traversed1) NIL)
   ((null traversed2) NIL)
   ((intersect traversed1 traversed2) (get-intersect traversed1 traversed2))
   (:else (let* 
              ((new-visited1 (append visited1 (list (car traversed1))))
               (new-visited2 (append visited2 (list (car traversed2))))
               (children1 (add-children8 graph (car traversed1)
                                         (append (cdr traversed1) new-visited1)))
               (children2 (add-children8 graph (car traversed2)
                                         (append (cdr traversed2) new-visited2)))
               (new-traversed1 (append (cdr traversed1) children1))
               (new-traversed2 (append (cdr traversed2) children2))
               (found-path (breadth-first-conc8 graph new-traversed1 new-traversed2
                                                new-visited1 new-visited2)))
            (cond
             ((null found-path) NIL)
             ((and (member (car (last found-path)) children2)
                   (member (car found-path) children1)) 
              (cons (car traversed1) (append found-path (list (car traversed2)))))
             ((member (car found-path) children1) (cons (car traversed1) found-path))
             ((member (car (last found-path)) children2) (append found-path
                                                                 (list (car traversed2))))
             (:else found-path))
            ))
   )
  )

(defun form-path(ls1 ls2)
  (append ls1 (reverse ls2))
  )

(defun intersect (ls1 ls2)
  (cond
   ((null ls1) NIL)
   ((null ls2) NIL)
   ((not (member (car ls1) ls2)) (intersect (cdr ls1) ls2))
   (:else T)
   )
  )

(defun get-intersect (ls1 ls2)
  (cond
   ((null ls1) NIL)
   ((null ls2) NIL)
   ((not (member (car ls1) ls2)) (get-intersect (cdr ls1) ls2))
   (:else (list (car ls1)))
   )
  )


