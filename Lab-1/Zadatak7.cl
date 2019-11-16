7. Napisati funkciju koja svaki od prvih n elemenata liste atoma uve?ava za 1, a preostale umanjuje za 1.
Zaglavlje funkcije: (promeni n lista)
Primeri poziva: (promeni ’4 ’(5 8 3 8 1 8 6 7 9)) => (6 9 4 9 0 7 5 6 8)

(defun promeni(n lista)
               (cond
                ((null lista) '())
                ((not (zerop n))(cons (1+(car lista))(promeni(- n 1)(cdr lista))))
                (t(cons(1-(car lista))(promeni n (cdr lista))))))