//stablo:
(setq graf '((A (B C)) (B (D E)) (C (F G)) (D (H I)) (E (I J)) (F (K)) (G (L M)) (H ()) (I ()) (J ()) 
             (K (A)) (L ( F)) (M ())))

//lakse za proveru graf:
(setq graf '((A (B)) (B (C D)) (C (G)) (D (F G)) (F (E)) (E ()) (G ())))
      

//glavna funkcija: za proveru da koliko disjunktnih grafova ima:
// pokrene se obilazak od pocetka,i vrati ceo graf,koji moze da se prodje odjednom
//nakon toga u promenljivu graf1 smesti se razlika izmedju citavog grafa i cvorovi koji su obradjeni
// uveca se vrednost za jedna i pozove se ova funkcija za novonastali graf (cvorovi koji nisu obradjeni)
(defun pozovi (graf)
  (cond ((null graf) 0)
        (:else(let* ((cvorovi1 (obilazak graf (list (caar graf) ) '()))
                     (graf1 (razlika graf cvorovi1)))
                (1+ (pozovi graf1))))))

//funkcija kojom se graf uporediti sa cvorovima koje smo obisli funkcijom za obilazak stabla
// i proverimo da li se element grafa nalazi u listi obradjenih,ako da,nastavi dalje da ispitujes
//ako se ne nalazi u listu koju funkcija vraca,stavi na prvo mesto cvor koji nije obradjen
(defun razlika (graf cvorovi)
  (cond ((null graf) '())
        ((member (caar graf) cvorovi) (razlika (cdr graf) cvorovi))
        (t(cons (car graf) (razlika (cdr graf) cvorovi)))))

//funkcija za obilazak stabla po sirini!
(defun obilazak (graf l cvorovi)
  (cond ((null l) cvorovi)
        (t(let* ((cvorovi1 (append cvorovi (list (car l))))
                 (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
                 (l1 (append (cdr l) potomci1)))
            (obilazak graf l1 cvorovi1)))))

//pomocne funkcije: bez dubine sada

(defun dodaj-potomke (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) cvor) (novi-cvorovi (cadar graf) cvorovi))
        (:else (dodaj-potomke (cdr graf) cvor cvorovi))))

(defun novi-cvorovi (potomci cvorovi)
  (cond ((null potomci) '())
        ((member (car potomci) cvorovi) (novi-cvorovi (cdr potomci) cvorovi))
        (:else (cons (car potomci) (novi-cvorovi (cdr potomci) cvorovi)))))