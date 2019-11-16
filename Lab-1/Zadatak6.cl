6. Napisati funkciju koja formira listu tako da svaki element ove liste odgovara broju elemenata prave liste koja je zadata kao ulazni parametar. Zadata lista se sastoji samo od podlisti
Zaglavlje funkcije: (prebroj lista)
Primeri poziva: (prebroj ’((a 1) (b 2) (c) (d 4 5) (e 6 (f 8))) ) => (2 2 1 3 3)

	(defun prebroj(lista)
               (cond
                ((null lista) '())
                (t(cons(length(car lista))(prebroj(cdr lista))))))