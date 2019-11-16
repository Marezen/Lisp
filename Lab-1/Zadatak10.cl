9. Napisati funkciju koja odre?uje bit parnosti za listu atoma ?iji elementi su samo 1 i 0.
Zaglavlje funkcije: (bitp lista)
Primeri poziva: (bitp ’(1 0 0 1 0 1 1 1 0)) => 1
(bitp ’(1 0 0 1 0 1 1 1 01)) => 0

(defun bitp(lista)
               (cond
                ((null lista) 0)
                ((not(zerop(car lista)))(mod(1+(bitp(cdr lista)))2))
                (t(mod(+ 0 (bitp(cdr lista)))2))))