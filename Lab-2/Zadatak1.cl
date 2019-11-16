1. 
a)
 IZMESAJ, koja od dve sortirane liste formira novu, takodje sortiranu listu, pri
cemu se relacija poredjenja zadaje kao argument funkcije.
Primeri:
(izmesaj '(1 3 5) '(2 4 6) '<) -> (1 2 3 4 5 6)

NAJBOLJE Resenje:

(defun mmerge (l1 l2 op)
               (cond
                ((null l2) l1)
                ((null l1) l2)
                ((equal op '<) (sort (append l1 l2) #'<))
                ((equal op '>) (sort (append l1 l2) #'>))))
b)
OST, ciji su argumenti dve liste atoma, a vraca sve elemente prve liste koji se ne
nalaze u drugoj.
Primeri:
(ost '(1 3 5) '(1 2 3 4 5 6)) -> ()
(ost '(a b c d e f g) '(a d k)) -> (b c e f g)

Resenje:

(defun ost (l1 l2)
  (cond ((null l1) '())
        ((null l2) (list l1))
        ((atom l1) (if (equal l1 (car l2)) '() (ost l1 (cdr l2))))
        (:else (append (ost (car l1) l2) (ost (cdr l1) l2)))))