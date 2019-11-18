(setq graf '((A (B C)) (B (D E)) (C (F G)) (D (H I)) (E (J)) (F (K)) (G (L M)) (H ()) (I ()) (J ()) 
             (K ()) (L ()) (M ())))

//stablo:
(setq graf '((A (B C)) (B (D E)) (C (F G)) (D (H I)) (E (I J)) (F (K)) (G (L M)) (H ()) (I ()) (J ()) 
             (K (A)) (L ( F)) (M ())))

//Za prezentacije:
(setq graf '( (A (B C)) (B (D E)) (C (G F)) (D (H)) (E (I G)) (F (J)) (H ()) (I (J)) (G (J)) (J ())))

//lakse za proveru graf:
(setq graf '((A (B)) (B (C D)) (C (G)) (D (F G)) (F (E)) (E ()) (G ())))

//potrebna je funkcija za obilazak po dubini,nadje visinu svakog cvora,i onda koristi "max" naredbu
//Mora da se promeni,dakle prosledjuje se NIVO na kom se trazi:
// nema cilj,mora da obidje celo stablo:
//da pristupimo cvoru,mora caar,zato sto prosledjujemo kao podlistu (A 1),gde je prvi clan cvor ,a drugi je njegov nivo
(defun nadji-put (graf l cvorovi)
  (cond ((null l) cvorovi)
        (t (let* ((cvorovi1 (cons (car l) cvorovi))
                  (potomci1 (dodaj-potomke graf (caar l) (append (cdr l) cvorovi1) (1+ (cadar l))))
                  (l1 (append potomci1 (cdr l))))
                 (nadji-put graf l1 cvorovi1)))))

//Samo promenimo tako sto za svaki poziv prosledimo i nivo
(defun dodaj-potomke (graf cvor cvorovi nivo)
	(cond	((null graf) '())
			((equal (caar graf) cvor) (novi-cvorovi(cadar graf) cvorovi nivo)) ;cadar
			(t (dodaj-potomke (cdr graf) cvor cvorovi nivo))))

//isto kao i za dodaj-potomke,prosledjujemo NIVO
(defun novi-cvorovi (potomci cvorovi nivo)
	(cond	((null potomci) '())
			((member (car potomci) cvorovi) (novi-cvorovi(cdr potomci) cvorovi nivo))
       (t (cons (cons(car potomci) (list nivo))(novi-cvorovi(cdr potomci) cvorovi nivo)))))

//Funkcija za trazenje visine stabla:
(defun visina (cvorovi)
	(cond ((null cvorovi) '0) 
       (t (max (cadar cvorovi) (visina (cdr cvorovi) )))))


//DRUGI Kraci nacin gde funkciju za visinu/dubinu implementiramo umesto Nadji-put
(defun nadji_dubinu (graf l max cvorovi)
  (cond ((null l) max)
        (t(let* ((max1 (if (> (cadar l) max) (cadar l) max))
                 (cvorovi1 (append cvorovi (list (car l))))
                 (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
                 (l1 (append (cdr l) potomci1)))
            (nadji_dubinu graf l1 max1 cvorovi1)))))
//ostle dve:
(defun dodaj-potomke (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) (car cvor)) (novi-cvorovi (cadar graf) cvorovi (1+ (cadr cvor))))
        (:else (dodaj-potomke (cdr graf) cvor cvorovi))))
(defun novi-cvorovi (potomci cvorovi lvl)
  (cond ((null potomci) '())
        ((member (car potomci) cvorovi) (novi-cvorovi (cdr potomci) cvorovi lvl))
        (:else (cons (list (car potomci) lvl) (novi-cvorovi (cdr potomci) cvorovi lvl)))))



(nadji_dubinu graf '((A 1)) '0 '()) // poziv
(visina (nadji-put graf '((A 1)) '())) // za onu iznad