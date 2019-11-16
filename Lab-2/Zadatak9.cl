9.
Definisati funkcije:
a. 
uporedi, koja upore?uje elemente podlisti asocijativne liste i odre?uje broj
podlisti u kojima je prvi element ve?i od drugog. Asocijativna lista ima po dva
numeri?ka atoma u svakoj od podlisti.
Primeri:
(uporedi '((2 1) (1 4) (4 2) (0 2) (3 1))) -> 3
(uporedi '((1 3) (2 0) (1 1) (2 5) (4 1) (3 3) (1 5))) -> 2

Resenje:

caar pristupi prvom elementu liste,a cadar pristupi drugom , onda se uporede,ako jeste ispunjen uslov,onda se vrati +1
(defun uporedi (lista)
               (cond
                ((null lista) '0)
                ((> (caar lista) (cadar lista)) (1+ (uporedi (cdr lista))))
                (t (uporedi (cdr lista)))))


b. uvecaj, koja uve?ava elemente prave liste za vrednost nivoa ugnježdavanja na
kom se nalaze. Prvi parametar predstavlja nivo koji se trenutno obra?uje.
Primeri:
(uvecaj '0 '(1 2 3 (4 5) 6)) -> (1 2 3 (5 6) 6)
(uvecaj '0 '((1 2) 3 ((4 (1 2) 5) 6))) -> ((2 3) 3 ((6 (4 5) 7) 7))

Resenje:

(defun uvecaj (level lista)
               (cond
                ((null lista) '())
                ((atom (car lista)) (cons (+ level (car lista)) (uvecaj level (cdr lista)))) 
                (t (cons (uvecaj (1+ level) (car lista)) 
				(uvecaj level (cdr lista))))))