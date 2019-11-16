4. Definisati funkcije:
a.
 pronadji, koja kao argumente ima klju? i asocijativnu listu, a vra?a vrednost
zadatog klju?a, odnosno () ako se on ne javlja u a-listi.
Primeri:
(pronadji 'a '((a 1)(b 2))) -> 1
(pronadji 'a '((b 2)(c 3))) -> ()

Resenje:
caar- uzme kljuc i elementa liste
cdar- vrati vrednost 
(defun pronadji (k alista)
                (cond
                 ((null alista) '())
                 ((equal k (caar alista)) (cdar alista))
                 (t (pronadji k (cdr alista)))))


b.
 umetni, za ubacivanje novog elementa ispred svakog pojavljivanja zadatog
elementa prave liste.
Primeri:
(umetni 'a 'x '(x y z w)) -> (a x y z w)
(umetni '(1 2 (3 4)) 'a '(a (b a f) j)) -> ((1 2 (3 4)) a (b (1 2 (3 4)) a f) j)

Resenje: malo duze
(defun umetni (novi zad lista)
              (cond
               ((null lista) '())
               ((equal zad (car lista)) (append (list novi)  (list (car lista)) (umetni novi zad (cdr lista))))
               ((atom (car lista)) (cons (car lista) (umetni novi zad (cdr lista))))
               (t (append (umetni novi zad (car lista)) (umetni novi zad (cdr lista))))))