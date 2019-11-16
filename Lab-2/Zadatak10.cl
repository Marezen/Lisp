10.Definisati funkcije:
a.
 napravi, koja pravi asocijativnu listu od liste klju?eva i liste vrednosti
uparivanjem odgovaraju?ih elemenata. Smatrati da je lista klju?eva dua ili
iste duine od liste vrednosti. Klju?eve koji nemaju odgovaraju?e vrednosti
uneti kao jedine elemente u asocijativnoj listi.
Primeri:
(napravi '(a b c d) '(1 2 3 (4 5))) -> ((a 1) (b 2) (c 3) (d (4 5))
(napravi '(a b c d e f g) '((1 2) 3 4 ((5 6) 7) 8)) -> ((a (1 2)) (b 3) (c 4) (d ((5 6) 7) (e 8) (f) (g))

Resenje:

(defun napravi (kljucevi vrednosti)
               (cond
                ((null kljucevi) '())
                ((null vrednosti) (cons (list (car kljucevi)) (napravi (cdr kljucevi) vrednosti)))
                (t (cons (list (car kljucevi) (car vrednosti)) (napravi (cdr kljucevi) (cdr vrednosti))))))
				
b. maks, koja odre?uje maksimalni element prave liste.
Primeri:
(maks '(1 5 8 9 4)) -> 9
(maks '((7 2) 3 5 ((9 6) 8) 1)) -> 9

Resenje:

(defun maks (lista)
               (cond
                ((null lista) '())
                ((equal (length lista) 1) (car lista)) 
                ((atom (car lista)) 
				(if (> (car lista) (maks (cdr lista))) (car lista) (maks (cdr lista))))
                (t (if (> (maks (car lista)) (maks (cdr lista))) (maks (car lista)) (maks (cdr lista))))))