6. Definisati funkcije:
a.
brpon, koja formira listu koja predstavlja broj ponavljanja zadatog simbola u
listi atoma. Isti simbol u listi se nalaze na uzastopnim pozicijama u listi. Prvi
parametar funkcije je pomo?na promenljiva za broj ponavljanja elementa.
Primeri:
(brpon '0 '(a a a b b c d d)) -> (3 2 1 2)
(brpon '0 '(a a a b b c d d d d e e)) -> (3 2 1 4 2)

Resenje:

(defun bpon (el lista)
  (cond ((null lista) '())
        ((equal (car lista) (cadr lista)) (bpon (1+ el) (cdr lista)))
        (t (cons (1+ el) (bpon '0 (cdr lista))))))
b.
 gldiag, koja izdvaja elemente sa glavne dijagonale kvadratne matrice u listu.
Matrica je zadata kao lista podlisti koje predstavljaju vrste matrice.
Primeri:
(gldiag '((1 2 3) (4 5 6) (7 8 9))) -> (1 5 9)
(gldiag '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))) -> (1 6 11 16)

Resenje:

(defun gldiag (lista)
               (cond
                ((null lista) '())
                (t (cons (nth (1- (length lista)) (reverse (car lista))) (gldiag (cdr lista))))))