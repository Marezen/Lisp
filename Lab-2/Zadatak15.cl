15.Definisati funkcije:
a)
 simetrican, koja proverava da li je zadati broj simetri?an. Broj je zadat kao
lista cifara. Prvi parametar funkcije je pomo?na lista. Nije dozvoljeno koristiti
funkciju nth i reverse.
Primeri:
(simetrican '() '(1 2 3 4 4 3 2 1)) -> t
(simetrican '() '(1 2 3 4 5 4 3 2 1)) -> t
(simetrican '() '(1 2 3 4 4 3 8 1)) -> ()

Resenje:
//sa Reverse:
(defun simetrican (lista)
  (cond ((null lista) t)
        ((equal (car lista) (car (reverse lista))) (simetrican (cdr (reverse (cdr lista)))))
        (t  '())))
		
		
//Main resenje:
(defun palindromp (a)
               (or (null a)
                   (null (cdr a))
                   (and (equal (car a) (car (last a)))
                        (palindromp (butlast (cdr a))))))
						
						
Neoptimalno resenje:
(defun sim (pom l)
  (cond
   ((null l) t)
   ((equal (length pom) (length l)) (and (equal (car pom) (car l)) (sim (cdr pom) (cdr l))))
   ((equal (length pom) (1- (length l))) (sim pom (cdr l)))
   (t (sim (cons (car l) pom) (cdr l))
      )
   )
  )


b) proizMV, koja izra?unava vektor jednak proizvodu matrice i vektora. Matrica
je zadata kao lista podlisti koje predstavljaju vrste matrice.
Primeri:
(proizMV '((1 2 3) (4 5 6) (7 8 9)) '(1 2 3)) -> '(14 32 50)
(proizMV '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)) '(1 2 2 1)) -> (15 39 63 87)

Resenje:

(defun proizMV (mat vect)
  (cond ((null vect) '0)    //ako je manji vektor od vrste,vrati 0
        ((null mat) '())
        ((atom (car mat)) (+ (* (car mat) (car vect)) (proizMV (cdr mat) (cdr vect))))
        (:else (cons (proizMV (car mat) vect) (proizMV (cdr mat) vect)))))
		
		(+(* (car mat) (car  vect)))