5. Definisati funkcije:
a)
 klju?evi, koja vra?a listu svih klju?eva zadate asocijativne liste.
Primeri:
(klju?evi '((a 1)(b 2))) -> (a b)

Resenje:
(defun kljucevi (alista)
               (cond
                ((null alista) '())
                (t (cons (caar alista) (kljucevi (cdr alista))))))



b)
 clan-p, koja vra?a T ako se zadati izraz pojavljuje kao element navedene prave
liste, a () u suprotnom slu?aju.
Primeri:
(clan-p 'a '(b a g)) -> t
(clan-p 'x '(a b c)) -> ()
(clan-p '(1 2) '((1 2 3) (4 (1 2) 5) 6)) -> t

Resenje:

(defun clan-p (el lista)
               (cond
                ((null lista) '())
                ((equalp el (car lista)))
                ((atom (car lista)) (clan-p el (cdr lista)))
                (t (or (clan-p el (car lista)) (clan-p el (cdr lista))))))