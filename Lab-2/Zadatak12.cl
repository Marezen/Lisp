12. Definisati funkcije:
a)
razlika, koja odre?uje vektor razlike dva zadata vektora.
Primeri:
(razlika '(4 8 2) '(3 5 7)) -> (1 3 -5)
(razlika '(4 6 1 6 9 3) '(6 2 0 7 4 3)) -> (-2 4 1 -1 5 0)

Resenje:

(defun razlika (l1 l2)
               (cond
                ((null l1) '()) // ako je prvi,onda ne stavlja rezultat oduzimanja
                ((null l2) l1) // ako desna lista ima manje elemenata od leve,onda ce se elementi leve samo prepisati
                (t (cons (- (car l1) (car l2)) (razlika (cdr l1) (cdr l2))))))

b)
 obrisi, koja briše n-ti element zadate liste i svih njenih podlisti, ukoliko
postoji. Prvi argument funkcije je pomo?ni indeks.
Primeri:
(obrisi ’0 '3 '(1 5 8 9 4)) -> (1 5 8 4)
(obrisi ’0 '3 '((7 2) 3 5 (8 5) ((9 6) 8 7 (0 9)) 1)) -> ((7 2) 3 5 ((9 6) 8 7) 1)

Resenje:

el se smanjuje dok ne pronadje vrednost indeksa elementa koji samo trerba da se preskoci.
(defun obrisi (el lista)
  (cond ((null lista) '())
        ( (= el 0)  (obrisi (1- el) (cdr lista)))
        (t( cons (car lista) (obrisi (1- el) (cdr lista))))))