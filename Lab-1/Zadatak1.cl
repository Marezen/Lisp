1. Napisati funkciju za dodavanje elementa u listu na n-toj poziciji.
Zaglavlje funkcije: (umetni el n lista)
Primeri poziva: (umetni ’d ’3 ’(a b c e f)) => (a b c d e f)
(umetni ’(d 4) ’3 ’((a 1) b c (e 5 (f 6))) ) => ((a 1) b c (d 4) (e 5 (f 6)))

(defun umetni(el n lista)
              (cond 
               ((null lista)(list el))
               ((zerop n)(cons el lista))
               ((not(minusp n))(cons(car lista)(umetni el (- n 1) (cdr lista))))))