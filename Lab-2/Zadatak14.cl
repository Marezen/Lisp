14.Definisati funkcije:
a)
 intenzitet, koja odre?uje kvadrat intenziteta zadatog vektora, kao zbir
kvadrata njegovih komponenti – x 1 2 + x 2 2 + ... + x n 2 .
Primeri:
(intenzitet '(4 5 6)) -> 77
(intenzitet '(5 6 7 8)) -> 174

Resenje:
(defun intezitet (lista)
  (cond
   ((null lista) '0)
   (t(+(*(car lista)(car lista) ) (intezitet (cdr lista))))))


b)
 postavi, koja dodeljuje vrednost elementu na zadatoj poziciji matrice. Matrica
je zadata kao lista podlisti koje predstavljaju vrste matrice.
Primeri:
(postavi '222 '1 '2 '((1 2 3) (4 5 6) (7 8 9))) -> '((1 2 3) (4 5 222) (7 8 9))
(postavi '222 '2 '3 '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))) -> ((1 2 3 4) (5 6 7 8) (9 10
11 222) (13 14 15 16))

Resenje:
 postavi (el v k lista)
               (cond
                ((null lista) '())
                ((> v 0) (cons (car lista) (postavi el (1- v) k (cdr lista))))
                ((> k 0) (cons (postavi el k 0 (car lista)) (cdr lista)))
                (t (cons el (cdr lista)))))