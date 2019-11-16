13.Napisati funkciju koja prolazi kroz listu i menja mesta susednim elementima ukoliko je
prethodnik manji od sledbenika.
Zaglavlje funkcije: (razmeni lista)
Primeri poziva: (razmeni ’(5 8 3 2 9 1 8 6 7)) => ’(8 5 3 9 2 8 6 7 1)				
				
(defun razmeni(lista)
  (cond
   ((null (cdr lista)) (list (car lista)))
   ((< (car lista) (cadr lista)) (append (list (cadr lista)) (razmeni (append (list (car lista)) (cddr lista)))))
   (T (append (list (car lista)) (razmeni (cdr lista))))))