(setq graf '((A (B)) (B (C D)) (C (G)) (D (F G)) (F (E)) (E ()) (G ())))


(defun formiraj_assoc (graf l cvorovi)
  (cond ((null l) (mapcar (lambda (x) (heuristika x )) (cdr cvorovi)))
        (t(let* ((cvorovi1 (append cvorovi (list (car l))))
                 (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
                 (l1 (append (cdr l) potomci1)))
            (formiraj_assoc graf l1 cvorovi1)))))

//i 2 pomocne funkcije kada radimo sa dubinom cvorova:

(defun dodaj-potomke (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) (car cvor)) (novi-cvorovi (cadar graf) cvorovi (1+ (cadr cvor))))
        (:else (dodaj-potomke (cdr graf) cvor cvorovi))))
//nije efikasno ovde da se vrsi oduzimanje Broj_Cvorova - dubina

(defun novi-cvorovi (potomci cvorovi lvl)
  (cond ((null potomci) '())
        ((member (car potomci) cvorovi) (novi-cvorovi (cdr potomci) cvorovi lvl))
        (:else (cons (list (car potomci) lvl) (novi-cvorovi (cdr potomci) cvorovi lvl)))))

(defun heuristika (cvor)
  (list (car cvor) (1- (cadr cvor))))


(formiraj_assoc graf '((a 1)) '())
(mapcar (lambda (x) (napravi x (length graf))) (cdr cvorovi))

(defun formiraj_assoc (graf l cvorovi)
  (cond ((null l) cvorovi)
        (t(let* ((cvorovi1 (append cvorovi (list (car l))))
                 (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
                 (l1 (append (cdr l) potomci1)))
            (formiraj_assoc graf l1 cvorovi1)))))

(ovo ce da vrati na kojoj dubini se nalazi,ako se to umanji za 1,onda ce se dobiti tacna i trazena heuristika)
