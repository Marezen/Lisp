8. Napisati funkciju za izdvajanje elemenata liste koji se nalaze na neparnim pozicijama u listi .
Zaglavlje funkcije: (neparni lista)
Primeri poziva: (neparni ’(a b c d e)) => (a c e)
(neparni ’((a 1) b c 6 (d 4 (e 5))) ) => ((a 1) c (d 4 (e 5)))

(defun neparni(lista)
               (cond
                ((null lista) '())
                (t(cons(car lista)(neparni (cdr(cdr lista)))))))