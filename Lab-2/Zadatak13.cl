13.Definisati funkcije:
a)
 polinom, koja odre?uje vrednost polinoma za zadatu vrednost promenljive x i
zadatu listu koeficijenta polinoma (od a 0 do a n ). Za izra?unavanje koristiti
Hornerovu šemu (a 0 +x·(a 1 +x·(a 2 +x·(a 3 +…+x·(a n-1 + x·a n )…)))). Nije
dozvoljeno koristiti funkciju exp.
Primeri:
(polinom '2 '(3 5 7)) -> 41
(polinom '2 '(6 2 0 7 4 3)) -> 226

Resenje:

(defun polinom (x coef)
  (cond ((equal (length coef) 1) (car coef))
        (:else (+ (car coef) (* x (polinom x (cdr coef)))))))

b)
 proizvod, koja odre?uje proizvod svih elemenata prave liste.
Primeri:
(proizvod '(1 2 3 (4 5))) -> 120
(proizvod '((1 2) 3 ((4 (1 2) 5) 6))) -> 1440

Resenje:

(defun proizvod (lista)
               (cond
                ((null lista) '1)
                ((atom (car lista)) (* (car lista) (proizvod (cdr lista))))
                (t (* (proizvod (car lista)) (proizvod (cdr lista))))))