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

//graf za testiranje A*
(setq graf '((A 9 ((B 4) (C 6)))
             (B 6 ((D 4) (A 2)))
             (C 2 ((D 4) (E 1)))
             (D 2 ((E 2) (F 3)))
             (E 3 ((F 4)))
             (F 0 (A 1))))
//POZIV (a-zvezda graf '(((A 9))) 'F)

// njega implementiramo na sl nacin: pomocu 8 funkcija, 1 glavna i 7 pomocnih
glavna funkcija a-zvezda izdvaja listu cvorova koji cine put od polaznog do ciljnog cvora

(defun a-zvezda (graf lp cilj)
  (cond ((null lp) '())
        ((equal (caar (last (car lp))) cilj) (car lp))
        (t (a-zvezda graf (nova-lista graf lp) cilj))))

//nova-lista - azurira listu parcijalnih puteva tako sto dodaje parcijalne nopve puteve i sortira u odgovarajucem redosledu po funkciji cene
(defun nova-lista(graf lp)
  (sortiraj(dodaj-puteve (nadji-puteve graf (car lp)) (cdr lp)) '<))

//sortiraj uredjuje listu parcijalnih puteva tako sto poziva funkciju dodaj za prvi put iz ove liste i vec uredjeni ostatak liste puteva
(defun sortiraj (lp op)
  (cond ((null lp) '())
        (t (dodaj (car lp) (sortiraj (cdr lp) op) op)))) 
//dodaj - umetne putanju u uredjenu listu puteva tako sto koristi operator za poredjenje vrednosti funkcije cene

(defun dodaj (put lp op)
  (cond ((null lp) (list put))
        ((apply op (list (cadar (last put))
                         (cadar (last (car lp))))) (cons put lp))
        (t (cons (car lp) (dodaj put (cdr lp) op)))))
//nadji-puteve: Pripremi parametre za odredjivanje parcijalnih puteva koji nastavljaju trenutni put,pozivajuci je za praznu listu novih parcijalnih puteva

(defun nadji-puteve (graf put)
  (let* ((cvor (caar (last put)))
         (cvor_ceo (assoc cvor graf))
         (g (- (cadar (last put)) (cadr cvor_ceo))))
    (nastavi-put '() put g (caddr cvor_ceo) graf)))

//nastavi-put - lista parcijalnih putanja tako sot doda potomke na put i naravno izracuna funkciju cene (F)
(defun nastavi-put (lp put g potomci graf)
  (cond ((null potomci) lp)
        (t (nastavi-put (cons (append put
                                      (list (list (caar potomci)
                                                  (+ g (cadr (assoc (caar potomci) graf))
                                                     (cadar potomci))))) lp)
                        put g (cdr potomci) graf))))

//dodaj-puteve - azurira listu parc. puteva tako sto doda po jedan parc. put u listu postojecih
(defun dodaj-puteve (putevi lp)
  (cond ((null putevi) lp)
        (t (dodaj-puteve (cdr putevi) (dodaj-put (car putevi) lp)))))

//dodaj-put dodaje novi parcijalni put u postojecu listu parcijalnih puteva,samo ako ne postoji vec takav ili sa vecom cenom puta
(defun dodaj-put (put lp)
  (cond ((null lp) (list put))
        ((and (equal (caar (last put)) (caar (last (car lp))))
              (< (cadar (last put)) (cadar (last (car lp)))))
         (cons put (cdr lp)))
        (t (cons (car lp) (dodaj-put put (cdr lp))))))















