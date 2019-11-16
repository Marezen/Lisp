// Graf sa slike:
(setq graf '((A (B C)) (B (D E)) (C (F G)) (D (H I)) (E (J)) (F (K)) (G (L M)) (H ()) (I ()) (J ()) 
             (K ()) (L ()) (M ())))

//stablo:
(setq graf '((A (B C)) (B (D E)) (C (F G)) (D (H I)) (E (I J)) (F (K)) (G (L M)) (H ()) (I ()) (J ()) 
             (K (A)) (L ( F)) (M ())))

//lakse za proveru graf:
(setq graf '((A (B)) (B (C D)) (C (G)) (D (F G)) (F (E)) (E ()) (G ())))

// funkcija koja vraca sve cvorove stabla u obliku liste
(defun izdvoji (graf)
(if (null graf) '()
  (cons (caar graf) (izdvoji (cdr graf)))))


//glavna funkcija za stablo koja se koristi prilikom trazenje u stablu(grafu) je "nadji-put"
//Ova funkcija izdvaja listu cvorova koji cine put od polaznog do ciljnog cvora
//(nadji-put graf '(start) 'cilj '())
//trazenje po sirini:
(defun nadji-put (graf l cilj cvorovi)
  (cond ((null l) nil)
        ((equal (car l) cilj) (list cilj))
        (t (let* ((cvorovi1 (append cvorovi(list(car l))))
                  (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
                  (l1 (append (cdr l) potomci1))
                  (nadjeni-put (nadji-put graf l1 cilj cvorovi1)))
           (cond ((null nadjeni-put) '())
                 ((member (car nadjeni-put) potomci1) (cons(car l) nadjeni-put))
                 (t nadjeni-put))))))

//trazenje po dubini:
  (defun nadji-put (graf l cilj cvorovi)
  (cond ((null l) nil)
        ((equal (car l) cilj) (list cilj))
        (t (let* ((cvorovi1 (append cvorovi(list(car l))))
                  (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
                  (l1 (append potomci1 (cdr l)))
                  (nadjeni-put (nadji-put graf l1 cilj cvorovi1)))
           (cond ((null nadjeni-put) '())
                 ((member (car nadjeni-put) potomci1) (cons(car l) nadjeni-put))
                 (t nadjeni-put))))))
 
//POMOCNA FUNKCIJA: "dodaj_potomke" izdvaja listu cvorova potomaka zadatog cvora koji do poziva funkcije nisu obradjeni
(defun dodaj-potomke (graf cvor cvorovi)
	(cond	((null graf) '())
			((equal (caar graf) cvor) (novi-cvorovi(cadar graf) cvorovi)) ;cadar
			(t (dodaj-potomke (cdr graf) cvor cvorovi))))


//JOS JEDNA POMOCNA FUNKCIJA: "novi-potomci" vraca listu koja ima potomke koji do poziva funkcije nisu obradjeni algoritmom
(defun novi-cvorovi(potomci cvorovi)
  (cond((null potomci) nil)
        ((member (car potomci) cvorovi)
         (novi-cvorovi(cdr potomci) cvorovi))
        (t(cons (car potomci) (novi-cvorovi(cdr potomci) cvorovi)))))
		
//obilazak stabla koji ce da vrati svaki cvor i dubina na kojoj se on nalazi!
(defun nadji-put (graf l cvorovi)
  (cond ((null l) cvorovi)
        (t (let* ((cvorovi1 (cons (car l) cvorovi))
                  (potomci1 (dodaj-potomke graf (caar l) (append (cdr l) cvorovi1) (1+ (cadar l))))
                  (l1 (append potomci1 (cdr l))))
                 (nadji-put graf l1 cvorovi1)))))
//poziv (nadji-put graf '((a 1)) '()) // root cvor i dubinu na kojoj se nalazi
  

// izdvajanje listova stabla
(defun izdvojiL(graf)
              (cond
               ((null graf) '())
               ((null (cadar graf))
                 (cons (caar graf) (izdvojiL(cdr graf)))
                (t(izdvojiL(cdr graf))))))

// Nalazenje prvog deteta:
(defun first-child (tree)
   (if (null tree)
      nil
      (caar tree))
  )
  
 // Da vidimo da li se odredjeni cvor nalazi u grafu ili stablu
(defun clan (cvor lista)
  (cond ((null lista) '())
        ((equal cvor (caar lista)) t)
        (:else (clan cvor (cdr lista)))))



// vraca sledbenike odredjenog cvora:
(defun sledbenik(cvor stablo)
  (cond
   ((null stablo) '())
   ((equalp (caar stablo) cvor) (cadar stablo))
   (t(sledbenik cvor (cdr stablo)))))


//Pretrage na osnovu heuristike, u obliku (Cvor Heuritistika)
PLANINARENJE: uredjena lista potomaka koja se doda na pocetak liste neobradjenih cvorova i na osnovu njih se sortira
BEST-FIT : lista potomaka se doda u listu neobradjenih cvorova a zatim se sortira na osnovu heuristike cvorova
ima 1 glavnu funkciju i 5 pomocnih;

//PLANINARENJE ON PRONADJE PRVI MOGUCI PUT
//BEST-FIT ustvari pronadje putanju koja je najbolja,na osnovu heuristickih vrednosti

//graf za proveru:
(setq graf1 '((a 2 (b)) (b 1 (c)) (c 0 (a b))))
//glavna: implementirano kao planinarenje: ako hocemo best fir onda treba samo (l1 (sortiraj(append potomci1 (cdr l)) '<)))) ))
(defun nadji-put(graf l cilj cvorovi)
  (cond
   ((null l) '())
   ((equal (caar l) cilj) (list (car l)))
   (t (let*((cvorovi1 (append cvorovi (list(car l))))
            (potomci1 (sortiraj(pridruzi graf (dodaj-potomke graf (caar l) (append l cvorovi1))) '<))
            (l1 (append potomci1 (cdr l)))
            (nadjeni-put(nadji-put graf l1 cilj cvorovi1)))
        (cond ((null nadjeni-put) '())
              ((assoc(caar nadjeni-put) potomci1)
               (cons (car l) nadjeni-put))
              (t nadjeni-put))))))
//nakon toga 
funkcija sortiraj: sortira listu
(defun sortiraj (ls op)
  (cond
   ((null ls) '())
   (t(dodaj (car ls) (sortiraj (cdr ls) op) op))))

//funkcija dodaj - doda element u uredjenu listu
(defun dodaj (el ls op)
  (cond
   ((null ls) (list el))
   ((apply op (list (cadr el) (cadr (car ls)))) (cons el ls))
   (t(cons (car ls) (dodaj el (cdr ls) op)))))
//funkcija pridruzi: pridruzi vrednost heuristike svakom od cvorova
(defun pridruzi (graf ls)
  (cond
   ((null ls) '())
   (t(cons (list (car ls) (cadr (assoc (car ls) graf)))
           (pridruzi graf (cdr ls))))))
//dodaj-potomke:
(defun dodaj-potomke (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) cvor)
         (novi-cvorovi (caddar graf) cvorovi))
        (t (dodaj-potomke (cdr graf) cvor cvorovi))))

//novi-cvorovi/cvorovi koji cekaju na obradu:
(defun novi-cvorovi (potomci cvorovi)
  (cond ((null potomci) '())
        ((member (car potomci) cvorovi)
         (novi-cvorovi (cdr potomci) cvorovi))
        (t (cons (car potomci)
                 (novi-cvorovi (cdr potomci) cvorovi)))))

//i JOS JEDAN ALGORITAM TRAZENJA U STABLU JE A*
// njega implementiramo na sl nacin:

