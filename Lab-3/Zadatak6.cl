(setq graf '((A (B)) (B (C D)) (C (G)) (D (F G)) (F (E)) (E ()) (G ())))
;Pozivi funkcija:
(napravi-stablo graf '((A)) '1)
(napravi-stablo graf '((A)) '2)

;Da bismo dobili stablo u orijentisanom grafu,potrebno je da vrati u obliku liste koje ima podliste cvorova koji mogu da se obidju kada se krene i polaznog cvora
;glavna funkcija,koja kao argument ima graf , polazni cvor i algoritam kojim ce se raditi
(defun napravi-stablo (graf start algoritam)
  (cond
   ((null graf) '())
   ((=(length graf) 1) graf)
   ((= algoritam 1) (princ "Obilazak po sirini")(obilazakSirina graf start '())) ; klasicno poziva obilazak po Sirini
   ((= algoritam 2) (princ "Obilazak po dubini")(obilazakDubina graf start '())))) ; a ovde po dubini

(defun obilazakSirina (graf l cvorovi)
  (cond ((null l) cvorovi)
        (t(let* ((cvorovi1 (append cvorovi (list (car l)))) ; cvorovi koje smo obisli,dakle ubacimo cvor koji smo obisli u listu obradjenih
                 (potomci1 (dodaj-potomke graf (caar l) (append l cvorovi1))) ; za cvor koji smo obradili,dodamo njegove potomke u listu koju "start",dakle to su cvorovi koje u sledecoj rekurziji obilazimo
                 (l1 (append (cdr l) potomci1)) ; pomeri se polazni cvor na prvog od potomka, 
                 (put (obilazakSirina graf l1 cvorovi1)))  ; i ovde pravi put
            (cond ((null put) '())
                   ((member (caar put) potomci1) (cons (list (car start)) put)) ; ovde je bitno sta funkcija vraca,(cons (list (car start)) put) , vrati cvor koji obidjemo u obliku podliste,i onda sledeci element je ustvari podlista koja sadrzi cvorove u koje moze da ode.(B C ) pa ce onda sledeci element da budu deca za B pa sl. za C itd.
                  (t put))))))

(defun obilazakDubina (graf l cvorovi)
  (cond ((null l) cvorovi)
        (t(let* ((cvorovi1 (append cvorovi (list (car l))))
                 (potomci1 (dodaj-potomke graf (caar l) (append l cvorovi1)))
                 (l1 (append potomci1 (cdr l)))
                 (put (obilazakDubina graf l1 cvorovi1)))
            (cond ((null put) '())
                   ((member (caar put) potomci1) (cons (list (car start)) put))
                  (t put))))))

; ovo su funkcije sa prezentacije,za obilazak stabla
(defun dodaj-potomke (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) cvor) (list(novi-cvorovi (cadar graf) cvorovi)))
        (:else (dodaj-potomke (cdr graf) cvor cvorovi))))


(defun novi-cvorovi (potomci cvorovi)
  (cond ((null potomci) '())
        ((member (car potomci) cvorovi)
         (novi-cvorovi (cdr potomci) cvorovi))
        (t (cons (car potomci)
                 (novi-cvorovi (cdr potomci) cvorovi)))))