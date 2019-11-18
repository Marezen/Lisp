//stablo:
(setq graf '((A (B C)) (B (D E)) (C (F G)) (D (H I)) (E (I J)) (F (K)) (G (L M)) (H ()) (I ()) (J ()) 
             (K (A)) (L ( F)) (M ())))

//lakse za proveru graf:
(setq graf '((A (B)) (B (C D)) (C (G)) (D (F G)) (F (E)) (E ()) (G ())))

//sada i E ide u cvor I i C ide u J
// da bi bilo vise puteva do cvora:

//obilazak,prepravljen da pravo formira listu: tako sto uzima vrednost cvora i nakon toga veze njegovu dubinu:
(defun formiraj_assoc (graf l cvorovi)
  (cond ((null l) '())
        (t(let* ((cvorovi1 (append cvorovi (list (car l))))
                 (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
                 (l1 (append (cdr l) potomci1)))
            (cons (car l) (formiraj_assoc graf l1 cvorovi1))))))
//poslednja linija ustvari kreira podlistu i stavi je na pocetak (A 1),npr

//i 2 pomocne funkcije kada radimo sa dubinom cvorova:

(defun dodaj-potomke (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) (car cvor)) (novi-cvorovi (cadar graf) cvorovi (1+ (cadr cvor))))
        (:else (dodaj-potomke (cdr graf) cvor cvorovi))))

(defun novi-cvorovi (potomci cvorovi lvl)
  (cond ((null potomci) nil)
        ((ispitaj (car potomci) cvorovi) (novi-cvorovi (cdr potomci) cvorovi lvl))
        (:else (cons (list (car potomci) lvl) (novi-cvorovi (cdr potomci) cvorovi lvl)))))

(defun ispitaj (cvor obradjeni)
  (cond ((null obradjeni) '())
        ((equal cvor (caar obradjeni)) t)
        (:else (ispitaj cvor (cdr obradjeni)))))

//POZIV: (formiraj_assoc graf '((A 1)) '())
