8.Definisati funkcije:
a.
 podeli, koja deli veliki broj, zapisan u obliku liste cifara, zadatim
jednocifrenim brojem.
Primeri:
(podeli '8 '(4 0 9 6)) -> (0 5 1 2)
(podeli '5 '(2 5 8 4 2 0 6 9 2 0)) -> (5 1 6 8 4 1 3 8 4)

Resenje:

(defun podeli (del broj)
                (cond
                 ((null (cdr broj)) (list (floor (car broj) del)))
                 (t (cons (floor (car broj) del) (podeli del (cons (+ (cadr broj) (* 10 (mod (car broj) del))) (cddr broj)))))))

b. 
unija, koja formira uniju elemenata dveju zadatih lista atoma.
Primeri:
(unija '() '(a b c)) -> (a b c)
(unija '(a b c) '(d a f)) -> (a b c d f)

Resenje:
(defun unija (l1 l2)
                (cond
                 ((null l1) l2)
                 ((null l2) (list l1))
                 ((listp l1) (append (unija (car l1) l2) (unija (cdr l1) l2)))
                 (t (if (equal l1 (car l2)) '() (unija l1 (cdr l2))))))