4. Napisati funkciju koja izdvaja pozitivne elemente iz liste atoma.
Zaglavlje funkcije: (pozitivni lista)
Primeri poziva: (pozitivni ’(2 -4 6 8 -7 5)) => (2 6 8 5)

(defun pozitivni(lista)
               (cond
                ((null lista) nil)
                 ((if(> (car lista) 1)
                      (cons(car lista)
                            (pozitivni(cdr lista)))))
                 (t(pozitivni(cdr lista)))))