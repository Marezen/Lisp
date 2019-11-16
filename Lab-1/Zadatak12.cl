12. Napisati funkciju koja u sortiranu listu atoma dodaje novi element na odgovaraju?e mesto.
Zaglavlje funkcije: (dodajs el lista)
Primeri poziva: (dodajs ’17 ’(1 3 6 8 14 20)) => (1 3 6 8 14 17 20)

(defun dodajs(el lista)
               (cond
                ((null lista)(list el))
                ((if (< el (car lista)) (cons el lista)))
                (t(cons(car lista)(dodajs el (cdr lista))))))