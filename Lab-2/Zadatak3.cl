3.
a)
pribavi, koja vra?a vrednost elementa na zadatoj poziciji matrice. Matrica je
zadata kao lista podlisti koje predstavljaju vrste matrice. Nije dozvoljeno
koristiti funkciju nth.
Primeri:
(pribavi '1 '2 '((1 2 3) (4 5 6) (7 8 9))) -> 6
(pribavi '2 '3 '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))) -> 12

Resenje:
Posto ne sme da koristi NTH funkcija,moramo mi da je napisemo:

(defun pribavi (vrsta kolona lista)
               (cond
                ((null lista) '())
                (t (pronadji kolona (pronadji vrsta lista)))))
(defun pronadji (p lista)
               (cond
                ((null lista) '())
                ((zerop p) (car lista))
                (t (pronadji (1- p) (cdr lista)))))

b) brdugih, koja odre?uje broj podlisti prave liste koje sadrže ve?i ili jednak broj
elemenata od zadatog.
Primeri:
(brdugih '3 '((1 2 3) (4 5) 6 7 (8 9 10 11))) -> 2
(brdugih '3 '((1 2 3) (1 2) 1 2 (1 (1 2 3) 3 (1 2) 5))) -> 3

Resenje:

(defun brdrugih (el lista)
  (cond ((null lista) 0)
        ((atom (car lista)) (brdrugih el (cdr lista)))
        ((>= (length (car lista)) el ) (+ 1 (brdrugih el (car lista)) (brdrugih el (cdr lista)))) 
        (t(+ (brdrugih el (car lista)) (brdrugih el (cdr lista))))))