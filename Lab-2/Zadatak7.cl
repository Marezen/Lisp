7.Definisati funkcije:
a. 
skupi, koja zamenjuje dva susedna elementa liste atoma njihovim zbirom.
Primeri:
(skupi '(1 2 3 4 5)) -> (3 5 7 9)
(skupi '(4 3 6 2 7 8 4)) -> (7 9 8 9 15 12)

Resenje:

(defun skupi (lista)
                (if (> (length lista) 1)
                    (cons (+ (car lista) (cadr lista)) (skupi (cdr lista)))))
					


b. skup-p, koja vra?a T ako je zadata lista skup (svaki element se pojavljuje
samo jednom), a () u suprotnom.
Primeri:
(skup-p '(a b c)) -> t
(skup-p '(a b a d)) -> ()
(skup-p '(a b (d f) (c (d f)))) -> ()


Resenje:

(defun skup-p (lista)
  (cond ((null (cdr lista)) t) 
        ((equal (car lista) (cadr lista)) '())        
        ((listp (cadr lista)) (and (skup-p (cons (car lista) (cadr lista))) (skup-p (cdr lista))))
        (t(and (skup-p (cons (car lista) (cddr lista))) (skup-p (cdr lista))))))