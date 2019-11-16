5. Napisati funkciju za izra?unavanje zbira vrednosti elementa liste atoma.
Zaglavlje funkcije: (zbir lista)
Primeri poziva: (zbir ’(2 3 7 6 5)) => 23

(defun zbir(lista)
               (cond
                ((null lista) 0)
                (t(+ (car lista) (zbir(cdr lista))))))