(setq graf '((A (B)) (B (C D)) (C (G)) (D (F G)) (F (E)) (E ()) (G ())))
;Pozivi funkcija:
(napravi-stablo graf '((A)) '1)
(napravi-stablo graf '((A)) '2)

(defun napravi-stablo (graf start algoritam)
  (cond
   ((null graf) '())
   ((=(length graf) 1) graf)
   ((= algoritam 1) (princ "Obilazak po sirini")(terpri)(obilazakSirina graf start '()))
   ((= algoritam 2) (princ "Obilazak po dubini")(terpri)(obilazakDubina graf start '()))))

(defun obilazakSirina (graf l cvorovi)
  (cond ((null l) cvorovi)
        (t(let* ((cvorovi1 (append cvorovi (list (car l))))
                 (potomci1 (dodaj-potomke graf (caar l) (append l cvorovi1)))
                 (l1 (append (cdr l) potomci1))
                 (put (obilazakSirina graf l1 cvorovi1)))
            (cond ((null put) '())
                   ((member (caar put) potomci1) (cons (list (car start)) put))
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