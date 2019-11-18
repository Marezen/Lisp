//stablo:
(setq graf '((A (B C)) (B (D E)) (C (F G)) (D (H I)) (E (I J)) (F (K)) (G (L M)) (H ()) (I ()) (J ()) 
             (K (A)) (L ( F)) (M ())))

//lakse za proveru graf:
(setq graf '((A (B)) (B (C D)) (C (G)) (D (F G)) (F (E)) (E ()) (G ())))
      
//da bi se dobilo stablo od grafa:

(defun napravi-stablo (graf cvorovi)
  (cond ((null cvorovi) graf)
        
//treba nam funkcija koja ce da izbaci cvor

//funkcija za obilazak stabla po sirini!
(defun obilazak (graf l cvorovi)
  (cond ((null l) cvorovi)
        (t(let* ((cvorovi1 (append cvorovi (list (car l))))
                 (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
                 (l1 (append (cdr l) potomci1)))
            (obilazak graf l1 cvorovi1)))))


(defun dodaj-potomke (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) cvor) (novi-cvorovi (cadar graf) cvorovi (car cvor)))
        (:else (dodaj-potomke (cdr graf) cvor cvorovi))))


//mora se promeniti malo,mora da se obezbedi da vraca roditelja
(defun novi-cvorovi (potomci cvorovi roditelj)
  (cond ((null potomci) '())
        ((clan (car potomci) cvorovi) (novi-cvorovi (cdr potomci) cvorovi roditelj))
        (:else (cons (list (car potomci) roditelj) (novi-cvorovi (cdr potomci) cvorovi roditelj)))))