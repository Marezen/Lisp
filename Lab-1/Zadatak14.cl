14. Napisati funkciju koja odre?uje broj parnih elemenata zadate liste atoma.
Zaglavlje funkcije: (brparni lista)
Primeri poziva: (brparni ’(5 8 3 2 9 1 8 6 7)) => 4

 (defun brparni(lista)
               (cond 
                ((null lista) 0)
                ((if( = (mod(car lista) 2) 0) (1+(brparni (cdr lista)))
                   (+ 0 (brparni( cdr lista)))))))