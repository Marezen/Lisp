2. Napisati funkciju za ispitivanje da li je zadata lista asocijativna?
Zaglavlje funkcije: (alistp alista)
Primeri poziva: (alistp ’((a 1) (b 2) (c 3) (d 4) (e 5)) ) => T
(alistp ’((a 1) (b 2) (c (3 4)) (d 4) ((e f) 2)) ) => T
(alistp ’((a 1) (b 2) c (d 4) (e 5)) ) => ()
(alistp ’((a 1) (b 2) (c) (d 4) (e 5)) ) => ()
(alistp ’((a 1) (b 2 8) (c 3)) (d 4)) ) => ()
			   
(defun alistp(lista)
              (cond
               ((null lista) t)
               ((atom(car lista)) '())
               ((not(= 2 (length(car lista)))) '())
               (t (alistp(cdr lista)))))