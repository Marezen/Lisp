3. Napisati funkciju koja odre?uje broj elemenata zadate liste. Ne koristiti funkciju lenght.
Zaglavlje funkcije: (duzina lista)
Primeri poziva: (duzina ’((a 1) b c 6 (d 4 (e 5))) ) => 5

(defun duzina(lista)
               (cond
                ((null lista) 0)
                ((not(null lista))(1+(duzina(cdr lista))))))