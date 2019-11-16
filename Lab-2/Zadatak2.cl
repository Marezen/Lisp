2.
a)
list+, za pokomponentno sabiranje elemenata dve liste.
Primeri:
(list+ '(1 2 3) '(4 5)) -> (5 7 3)

Resenje:
(defun list+ (l1 l2)
               (cond
                ((null l1) l2)
                ((null l2) l1)
                (t (cons (+ (car l1) (car l2)) (list+ (cdr l1) (cdr l2))))))
b)
broj_pojavljivanja, koja vra?a broj pojavljivanja zadatog izraza u pravoj listi.
Primeri:
(broj_pojavljivanja 'a '(1 2 a g 6)) -> 1
(broj_pojavljivanja 'a '(a (g a f) b a (k (l k a o) y))) -> 4
(broj_pojavljivanja '(1 2) '((1 2) 3 4 (5 (1 2) 6) 7 (1 2)) -> 3

Resenje:

(defun broj_pojavljivanja (el lista)
               (cond
                ((null lista) '0)
                ((equal el (car lista)) (1+ (broj_pojavljivanja el (cdr lista))))
                ((atom (car lista)) (broj_pojavljivanja el (cdr lista)))
                (t (+ (broj_pojavljivanja el (car lista)) (broj_pojavljivanja el (cdr lista))))))