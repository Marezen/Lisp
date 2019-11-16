//stablo:
(setq graf '((A (B C)) (B (D E)) (C (F G)) (D (H I)) (E (I J)) (F (K)) (G (L M)) (H ()) (I ()) (J ()) 
             (K (A)) (L ( F)) (M ())))

//lakse za proveru graf:
(setq graf '((A (B)) (B (C D)) (C (G)) (D (F G)) (F (E)) (E ()) (G ())))

//sada i E ide u cvor I i C ide u J
// da bi bilo vise puteva do cvora:

//obilazak:
(defun nadji-put (graf l cilj cvorovi)
  (cond ((null l) '())
        ((equal (car l) cilj) (list cilj))
        (t(let* ((cvorovi1 (append (list (car l)) cvorovi))
                 (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi)))
                 (l1 (append (cdr l) potomci1))
                 (nadjeni-put (nadji-put graf l1 cilj cvorovi1)))
            (cond ((null nadjeni-put) '())
                  ((member (car nadjeni-put) potomci1) (cons (car l) nadjeni-put))
                  (:else nadjeni-put))))))

//i 2 pomocne funkcije:
(defun dodaj-potomke (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) cvor) (novi-cvorovi (cadar graf) cvorovi))
        (t(dodaj-potomke (cdr graf) cvor cvorovi))))

(defun novi-cvorovi (potomci cvorovi)
  (cond ((null potomci) '())
        ((member (car potomci) cvorovi) (novi-cvorovi (cdr potomci) cvorovi))
        (t(cons (car potomci) (novi-cvorovi (cdr potomci) cvorovi)))))

//funkcije za odredjivanje broja puteva: Imamo pravo vise puta da pozovemo obilazak,dakle nalazimo putanju vise puta

//vraca sve putanje disjunktne putanje
(defun Putanje(graf start cilj)
  (cond ((null graf) '())
        (:else (let* ((put1 (nadji-put graf start cilj '()))
                      (graf1 (ocisti graf (cdr (reverse (cdr put1))))))
                 (cond ((null put1) '())
                       (:else (cons put1 (Putanje graf1 start cilj))))))))

//Ocisti funkcija,sluzi da obrise nadjenu putanju od start do cilja,da ne bismo,kroz ponovni obilazak,naisli na istu
(defun ocisti (graf lista)
    (cond ((null graf) '())
        ((member (caar graf) lista) (ocisti (cdr graf) lista))
          (:else (cons (car graf) (ocisti (cdr graf) lista)))))

// Da vrati broj putanja:
(defun broj_putanja (graf start cilj)
  (cond ((null graf) 0)
        (:else (let* ((put1 (nadji-put graf start cilj '()))
                      (graf1 (ocisti graf (cdr (reverse (cdr put1))))))
                 (cond ((null put1) 0)
                  (:else (1+ (broj_putanja graf1 start cilj))))))))