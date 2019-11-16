15. Napisati funkciju za izdvajanje atoma iz liste.
Zaglavlje funkcije: (atomi lista)
Primeri poziva: (atomi ’((a 1) b c 6 (d 4 (e 5))) ) => (b c 6)

(defun atomi(lista)
               (cond 
                ((null lista) nil)
                ((atom(car lista)) (cons (car lista) (atomi(cdr lista))))
                (t(atomi(cdr lista)))))