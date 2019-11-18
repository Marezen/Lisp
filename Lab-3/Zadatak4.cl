//stablo:
(setq graf '((A (B C)) (B (D E)) (C (F G)) (D (H I)) (E (I J)) (F (K)) (G (L M)) (H ()) (I ()) (J ()) 
             (K (A)) (L ( F)) (M ())))

//lakse za proveru graf:
(setq graf '((A (B)) (B (C D)) (C (G)) (D (F G)) (F (E)) (E ()) (G ())))
      
//A je samo za B , K je za A , i L je za F
//sada i E ide u cvor I i C ide u J
// da bi bilo vise puteva do cvora:

//koristimo slicnu funkciju kao u prethodnom zadatku za formiranje assoc. liste:
//samo moramo da upotrebimo lambda izraz
//logika: Prvo obidje citavo stablo,dakle formira listu obradjenih cvorova,to su cvorovi u kojima moze da se pristupi
//iz pocetnog/startnog cvora. Dakle dobijamo associjativnu listu (A 1) (B 2), itd.. 
//I kada se lista L isprazni,kada nema vise cvorova koje moze da obidje,
//nad konacnom listom koju smo dobili (obradjeni cvorovi) pozivamo mapcar,znaci svakom od njih se pristupi
//i pozove se funkcija "napravi" .. X ustvari uzme vrednost (A 1) pa (B 2) itd. i vraca kao listu 
//prvi element,to je dakle Cvor i onda sa cadr se pristupi njegovoj dubini,i ona se oduzme od ukupnog broja cvorova i dobijemo konacno heuristiku

(defun formiraj_assoc (graf l cvorovi)
  (cond ((null l) (mapcar (lambda (x) (heuristika x (length graf))) (cdr cvorovi)))
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
        ((ispitaj (car potomci) cvorovi) (novi-cvorovi (cdr potomci) cvorovi lvl))
        (:else (cons (list (car potomci) lvl) (novi-cvorovi (cdr potomci) cvorovi lvl)))))

(defun ispitaj (cvor obradjeni)
  (cond ((null obradjeni) '())
        ((equal cvor (caar obradjeni)) t)
        (:else (ispitaj cvor (cdr obradjeni)))))


(defun heuristika (cvor duzina)
  (list (car cvor) (- duzina (cadr cvor))))

(formiraj_assoc graf '((a 1)) '())

// Brzi nacin!:

(defun razlika (graf cvor potomci nivo obradjeni)
  (cond ((null cvor) 
         (if (null potomci) '()
           (razlika graf potomci cvor (+ 1 nivo) obradjeni)))
        (t (let* ((potomci1 (append potomci (dodaj-potomke graf (car cvor) (append obradjeni potomci cvor))))
                  (cvor1 (cdr cvor))
                  (obradjeni1 (append obradjeni (list (car cvor)))))
             (append (list (list (car cvor) (- (length graf) nivo))) (razlika graf cvor1 potomci1 nivo obradjeni1))))))

(defun dodaj-potomke (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) cvor)
         (novi-cvorovi (cadar graf) cvorovi))
        (t (dodaj-potomke (cdr graf) cvor cvorovi))))

(defun novi-cvorovi (potomci cvorovi)
  (cond ((null potomci) '())
        ((member (car potomci) cvorovi)
         (novi-cvorovi (cdr potomci) cvorovi))
        (t (cons (car potomci)
                 (novi-cvorovi (cdr potomci) cvorovi)))))


