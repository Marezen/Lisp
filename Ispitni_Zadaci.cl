;;;;   1.Napisati funkciju na Lispu dopuni  koja na osnovu zadate prave liste formira novu listu tako sto svakoj listi
na pocetku dodaje broj elemenata u njoj. Praznu listu tretirati kao atom.

;Poziv funkcije:
(dopuni ‘(a b (c d) e (f))) -> ‘(6 a b (2 c d) e (2 f))

(defun dopuni (lista)
  (cond
   ((null lista) nil)
   ((atom (car lista)) (cons (car lista) (dopuni(cdr lista))))
   ((not(atom (car lista))) (cons(pomocna (car lista)) (dopuni (cdr lista))))
   ))

(defun pomocna(lista)
  (cons (1+ (length lista)) lista)   ; 1+ jer smatramo da i '() jedan atom, i onda je zato sve za 1 vise.
  )


;;;;  2.Definisati funkciju rot-desno  koja rotira elemente liste L za n mesta udesno.

;Poziv funkcije:
(rot-desno 2 '(A B C D E)) -> (D E A B C)

(defun rot-desno (n lista)
  (cond
   ((null lista) '())
   ((atom lista) lista)
   ((equalp n 0) lista)
   (t  (rot-desno (1- n) (append  (last lista) (butlast lista))))
   )
  )


;;;;  3. Definisati funkciju rot-levo koja rotira elemente liste L za n mesta ulevo

;Poziv funkcije:
(rot-levo 3 '(A B C D E)) -> (D E A B C)

(defun rot-levo (n lista)
  (cond
   ((null lista) '())
   ((atom lista) lista)
   ((equalp n 0) lista)
   (t  (rot-levo (1- n) (append (cdr lista) (list (first lista)))))
   )
  )


;;;  4.Definisati funkciju : vrednost_al  koja iz ascoijativne liste al  za zadati kljuc k vraca odgovarajucu vrednost.

;Poziv funkcije:
(vrednost_al 'C '((A 1) (B 11) (C 111) (D 1111))) -> 111

(defun vrednost_al (k al)
  (cond
   ((null al) '())
  ; ((not(alistp al)) (format t "Nije asocijativna lista~%"))  ;; ovo moze da se doda za slucaj da ne unesemo asoc. listu kao argument.
   ((equalp k (caar al)) (cadar al))
   (t (vrednost_al k (cdr al)))
   )
  )

; Moze a i ne mora da postoji ovo. 
; Ova funkcija proverava da li je lista asocijativna..ako nije vrati false
(defun alistp(lista)
              (cond
               ((null lista) t)
               ((atom(car lista)) '())
               ((not(= 2 (length(car lista)))) '())
               (t (alistp(cdr lista)))))


;;;;  5.Napisati funkciju na Lispu formiraj koja ima 2 argumenta,graf u standardnom obliku I listu u standardnom obliku ,
;;;;     gde svaki element liste predstavlja tezinu cvora koja odgovara grani cvor-sled,
;;;;     I formira graf sa tezinama oblika ((cvor (sled1 tez1)) (cvor2 (sled2 tez2)) ..)

; Poziv funkcije:
(formiraj '((A (B C)) (B (D E)) (C (E))) '(5 10 15)) --> ((A (B C 5)) (B (D E 10)) (C (E 15)))


(defun formiraj (graf listaTezina)
 (cond
  ((null graf) nil)
  ((null listaTezina) nil)
  ((not(equalp (length graf) (length listaTezina))) (format t "Graf i zadata lista nisu istih dimenzija! ~%"))
  (t (cons (cons (caar graf) (list(append (cadar graf) (list(car listaTezina))))) (formiraj (cdr graf) (cdr listaTezina)))))
  )

;;prvi cons sluzi da dodaje novoFormirani oblik (A (B C 5)) u listu koja se formira (formiraj (cdr graf) (cdr listaTezina))
;;drugi cons sluzi da formira prethodno navedeni oblik, znaci doda A u listu koja se formira tako sto ce podlista (sled1 sled2) da se na kraj
;; stavi (car listaTezina),dakle doda tezinu samo, i dobijemo oblik (B C 5) , cons A na (B C 5 ) dobijamo -> (A (B C 5)) 


;;;;  6. Napisati funkciju formiraj na Lispu koja formira reprezentaciju grafa u standardnom obliku na osnovu grafa koji je predstavljen 
;;;;     listom grana. 

;Poziv funkcije:
(formiraj '((a b) (a c) (b c) (b e) (c a) (c d) (c f) (d f))) -> ((a (b c)) (b (c e)) (c (a d f)) (d (f)) (f ()))

;; samo ovako sam uspeo da odradim:

(defun formiraj(lista)
  (cond 
   ((null lista) '())
   (t (cons (cons (caar lista) (list(napraviCvor (caar lista) lista))) (formiraj (izdvojiListu (caar lista) lista))))
   )
  )

(defun napraviCvor (cvor lista)
  (cond
   ((null lista) '())
   ((equalp (caar lista) cvor) (cons (cadar lista) (napraviCvor cvor (cdr lista))))
   (t (napraviCvor cvor (cdr lista)))
   )
  )

(defun izdvojiListu (cvor lista)
  (cond
   ((null lista) nil)
   ((equalp (caar lista) cvor) (izdvojiListu cvor  (cdr lista)))
   (t (cons (car lista) (IzdvojiListu cvor (cdr lista))))
   )
  )

;;;;  7. Definisati funkciju dif_razlika l1 l2 na Lisp-u koja odredjuje diferencijalnu razliku skupova. 

;Poziv funkcije:
(dif_razlika '(1 7 2 9 6 3) '(4 2 6 11 8 9)) -> (1 7 3 4 11 8)

;Mozemo ovako da napisemo,ali to nece da vrati elemente u redosledu kojim su ona zadali,ali svakako ce vratiti listu elemenata koji se ne ponavljaju
(defun dif_razlika (l1 l2)
  (append (set-difference l1 l2 :test #'equal) (set-difference l2 l1 :test #'equal))) 


;;mora da ispita za obe liste l1 prema l2 i obrnuto
(defun dif_razlika (l1 l2)
  (append (razlika l1 l2) (razlika l2 l1)))

;;treba da dodjemo do 1 elementa liste l1 i onda uporedjujemo redom sa elementima druge liste
;; ako se element iz l1 nadje u l2, onda ce da vrati '() ,ako ne onda zove razliku za ostatak liste l2 ,
;; ako se ne poklopi element l1 ni sa jednim iz l2 onda ce funkcija razlika da vrati taj element,i onda njega stavlja u listu
(defun razlika(l1 l2)
  (cond
   ((null l1) '())
   ((null l2) (list l1))
   ((listp l1) (append (razlika (car l1) l2) (razlika (cdr l1) l2)))
   (t (if (equalp l1 (car l2)) '() ; then
        (razlika l1 (cdr l2))))))  ; else


;;;;  8.  Napisati funkciju u Lispu koja odredjuje broj listova u stablu. Stablo je predstavljeno asocijativnom listom.

;Test stablo:
(setq stablo '((c1 (c2 c3)) (c2 (c4 c5)) (c3 (c6 c7 c8)) (c8 (c9)))
      
;Poziv funkcije:
(broj_listova stablo) -> 5
    
;;alternativno resenje:
(defun br_lista (lista)
  (br_lista_1 lista lista))

(defun br_lista_1 (lista dlista)
  (cond ((null lista) 0)
        (t (+ (proveri (cadar lista) dlista) (br_lista_1 (cdr lista) dlista)))))

(defun proveri (potomci dlista)
  (cond ((null potomci) 0)
        ((assoc (car potomci) dlista) (proveri (cdr potomci) dlista)) ; proverava za svaki cvor (C1 (C2 C3)) to su 2 elementa
        (t(1+ (proveri (cdr potomci) dlista)))))


;;;;   9. Definisati funkciju na Lispu zamena koja elemente liste zamenjuje odgovarajucim elementima Iz asocijativne liste al.
;;;;      Ukoliko element osnovne liste l ne postoji u asocijativnoj listi al ne treba ga menjati. 

;Poziv funkcije:
(zamena '(1 6 (7 2 (5 8)) 3 (4 9)) '((2 a) (1 c) (3 b) (4 e))) -> (c 6 (7 a (5 8)) b (e 9))

;;u ovoj funkciji u (t ()) slucaju za cond, je situacija kada naidje na podlistu.

(defun zamena(lista alista)
  (cond
   ((null lista) '())
   ((null alista) lista)  ; ovo i nije neophodno,radi lepseg rada funkcije.
   ((atom (car lista)) (cons (smena (car lista) alista) (zamena (cdr lista) alista)))
   (t (append (list(zamena (car lista) alista)) (zamena (cdr lista) alista)))
   )
  )
;mora postojati ova dodatna funkcija
(defun smena(element alista)
  (cond
   ((null alista) element)
   ((equalp element (caar alista)) (cadar alista))
   (t (smena element (cdr alista)))))



;;;;  10. Napisati Lisp funkciju kvadrat koja vrsi zamenu svih elemenata u pravoj listi sa odgovarajucim elementom koji predstavlja
;;;;      kvadrat elementa koji se menja. 
;;;;      Ako je kao argument navedena prava lista,funckija treba da omoguci zamenu I elemenata podlisti na isti nacin.

;Poziv funkcije:
(kvadrat '(1 2 4 (1 (2) 3))) -> '(1 4 16 (1 (4) 9))

(defun kvadrat(lista)
  (cond
   ((null lista) '())
   ((atom (car lista)) (cons (expt (car lista) 2) (kvadrat (cdr lista))))
   (t (append (list(kvadrat (car lista))) (kvadrat (cdr lista))))
   )
  )

;;;;  11. Napisati Lisp funkciju insert za ubacivanje novog elementa ispred svakog pojavljivanja zadatog elementa prave liste. 

;Poziv funkcije:
(insert 'a 'x '(x y z w)) -> (a x y z w)
(insert 'a 'z '(x y z w z)) -> (x y a z w a z)

(defun insert (el posle lista)
  (cond
   ((null lista) '())
   ((equalp posle (car lista)) (cons el (cons posle (insert el posle (cdr lista)))))
   (t (cons (car lista) (insert el posle (cdr lista))))
   ))

;;;;  12. Napisati funkciju na Lispu ODREDI koja za zadati graf odredjuje maksimalni ulazni stepen:

;Poziv funkcije:
(odredi '((a (a b c)) (b (c d)) (c ()) (d (a b)))) -> 2
(odredi '((a (a b d)) (b (c d)) (c (d)) (d ()))) -> 3

;;nisam nikako drugacije uspeo da sredim,moralo je sa 3 funkcije,gde ova prva bukvalno nema neki smisao
; moze da se resi tako sto bismo za UlazniStepen koristili globalnu promenljivu graf

(defun odredi (graf)
  (cond
  ((null graf) '0)
  (t (odredi_pomocna graf graf)))
  )

(defun odredi_pomocna(graf zaProveru)
  (cond
   ((null graf) '0)
   (t (max (ulazniStepen (caar graf) zaProveru) (odredi_pomocna (cdr graf) zaProveru)))
  )
  )

(defun ulazniStepen (element lista)
  (cond
   ((null lista) '0)
   ((member element (cadar lista)) (1+ (ulazniStepen element (cdr lista))))
   (t (ulazniStepen element (cdr lista)))
   )
  )
        

;;;; 13. Napisati funkciju na Lispu popuni koji ima 2 argumenta, listu koja se sastoji od listi I listu atoma,
;;;;     I koja formira novu listu tako sto umesto praznih mesta (-) u listi redom postavlja elemente iz lista atoma sve dok ne stigne do
;;;;     kraja prve ili do kraja druge liste.Prvi element svake podliste u prvoj listi je broj praznih mesta
;;;;     I njega treba setovati na odgovarajucu vrednost pri izlasku iz funkcije.

;Poziv funkcije:
(popuni '((1 a - b c) (2 - - d e f) ) '(x y z)) -> ((0 a x b c) (0 y z d e f))
(popuni '((1 a - b c) (5 - - - - -) ) '(x y z d)) -> ((0 A X B C) (2 Y Z D - -))

;;radimo sa logikom da popunjavamo element po element,sa (cdr popuna) praznimo elemente koji su na raspolaganju za popunjavanje
(defun popuni (lista popuna)
  (cond
   ((null lista) lista)
   ((null popuna) lista)
   ((equal 0 (caar lista)) (cons (car lista) (popuni (cdr lista) popuna)))  ;ako nema praznih polja samo vrati tu podlistu i pozove za ostatak
   (t (popuni (cons (popuni-el (car lista) popuna) (cdr lista)) (cdr popuna)))
   ))


(defun popuni-el (lista popuna)
  (cond
   ;; Potrebno je da prvi element,koji je BROJ smanjimo za jedan,jer dodajemo nov element na prazno polje.
   ((numberp (car lista)) (cons (1- (car lista)) (popuni-el (cdr lista) popuna)))
   ((equal (car lista) '-) (cons (car popuna) (cdr lista)))
   (t (cons (car lista) (popuni-el (cdr lista) popuna))) ;; ako ima normalno element,samo ga ubaci i nastavi dalje za ostatak liste.
   ))


;;;;  14. Napisati funkciju na Lispu formiraj koja ima 1 argument kao asocijativnu listu , ciji su elementi oblika (kljuc vrednost). 
;;;;      Na osnovu zadate liste,funkcija formira novu asocijativnu listu kod koje su zamenjena mesta kljucu I vrednosti u svakom element
;;;;      asocijativne liste.

;Poziv funkcije:
(formiraj '((1 a) (2 b) (3 c) (4 d) (5 e))) -> ((a 1) (b 2 ) (c 3) (d 4) (e 5))

(defun formiraj (alista)
  (cond
   ((null alista) '())
   ((> (length (car alista)) 2) (format t "Nije asocijativna lista!"))
   (t (cons (reverse (car alista)) (formiraj (cdr alista))))))



;;;;  15. Definisati Lisp funkciju (proizvod mat vek) koja izracunava vector jednak proizvodu matrice I vektora. 
;;;;      Matrica je zadata kao lista podlisti,gde je svaka podlista vrsta matrice. Vektor je lista atoma.

;Poziv funkcije:
(proizvod '((1 2 3) (4 5 6) (7 8 9)) '(1 2 3))  -> (14 32 50)

(defun proizvod (mat vek)
  (cond
   ((null mat) '())
   ((null vek) mat)
   ((not(equalp (length (car mat)) (length vek))) (format t "~% Dimenzije moraju biti isteee! ~%"))
   (t (append (list(mnozi (car mat) vek)) (proizvod (cdr mat) vek)))))

(defun mnozi(vek1 vek2)
  (cond
   ((not (or(null vek1) (null vek2))) (+ (* (car vek1) (car vek2)) (mnozi (cdr vek1) (cdr vek2))))
   (t 0)))


;;;;  16. Napisati Lisp funkciju zamena koja vrsi zamenu onih elemenata liste zadatim elementom,koji predstavljaju zbir prethodnog I 
;;;;      sledeceg elementa u listi. Prvi I poslednji element se zbog zadatog uslova ne zamenjuju. Funkcija ima 2 argumenta,ulaznu listu atoma,
;;;;      I element kao zamenu , a kao izlaz daje modifikovanu listu.

;Poziv funkcije:
(zamena '(1 8 7 9 2 4 3 4 1) 17) -> (1 17 7 17 2 4 3 17 1)

(defun zamena(lista broj)
  (cond
   ((null lista) '())
   ((null (nth 1 lista)) (cons (nth 0 lista) (zamena (cdr lista) broj)))
   ((equalp (nth 1 lista) (+ (nth 0 lista) (nth 2 lista))) (cons (nth 0 lista) (cons broj (zamena (cddr lista) broj))))
   (t (cons (car lista) (zamena (cdr lista) broj)))
   )
  )




;;;;  17. Napisati Lisp funkciju zamena koja vrsi zamenu svih pojavljivanja zadatog elementa u listi atoma proizvodom vrednosti tog i
;;;;      sledeceg clana liste (u odnosu na pronadjenu poziciju zadatog elementa). Ako je pozicija trazenog elementa na kraju liste,
;;;;      odnosno iza njega ne postoji element, nova vrednost je kvadrat vrednosti zadatog broja. 
;;;;      Funkcija ima dva argumenta, ulaznu listu atoma (brojeva), i zadati element (broj) a kao izlaz daje modifikovanu listu

;Poziv funkcije:
(zamena '(5 8 7 6 5 4 3 2 5) 5) => (40 8 7 6 20 4 3 2 25)


(defun zamena (lista el)
  (cond ((null lista ) '())
        ((equal (car lista) el) (if (null (cdr lista)) (list(* el el)) (cons (* el (cadr lista)) (zamena (cdr lista) el))))
        (t (cons (car lista) (zamena (cdr lista) el)))))



;;;;  18. Napisati funkciju na Lispu (rotiraj Lista element n) koja ima 3 argumenta. Pravu listu ciji sus vi elementi liste,
;;;;      element e I broj n. Elementi podlisti se tretiraju kao Celina. 
;;;;      Funkcija treba da izvrsi rotiranje podlisti liste L za n mesta ULEVO,ali samo pod uslovodom da se u podlisti pojavljuje
;;;;      zadati element e.

;Poziv funkcije:
(rotiraj '((1 2 3 4 5) (a b c1 c2 c3 6) (1 a c 4 5 f)) 'a 2) -> ((1 2 3 4 5) (c1 c2 c3 6 a b) (c 4 5 f 1 a))


(defun rotiraj (lista element n)
  (cond
   ((null lista) '())
   ((atom (car lista)) (format t "~% Nesto nije u redu,lista mora imati samo podliste!~%"))
   ((not(null(member element (car lista)))) (cons (list (rot-levo n (car lista))) (rotiraj (cdr lista) element n)))
   (t (cons (car lista) (rotiraj (cdr lista) element n)))
   ))

;Upotrebicemo vec definisanu funkciju gore,za rotiranje za N mesta ulevo
(defun rot-levo (n lista)
  (cond
   ((null lista) '())
   ((atom lista) lista)
   ((equalp n 0) lista)
   (t  (rot-levo (1- n) (append (cdr lista) (list (first lista)))))
   )
  )



;;;;   19. Definisati funkciju obrisi  na programskom jeziku Lisp koja rekurzivno brise sve podliste ciji zbir elemenata je manji
;;;;       od zadate vrednosti, sve dok postoje podliste sa navedenim svojstvom.

;Poziv funkcije:
(obrisi 2 '(3 (-7 4) 5 -4 2 (2 -1 3) (9 (4 -6 2) -3 (1 2)))) -> (3 5 -4 2 (2 -1 3)(9 -3 1 3))

;; Nisam zavrsio ovu do kraja.

(defun obrisi (granica lista)
  (cond
   ((null lista) '())
   ((atom
   ((listp (car lista))  (obrisi granica (cdr lista))))
   (t (cons (car lista) (obrisi granica (cdr lista))))
   )
  )
  
  ;; ???
(defun Suma (lista)
  (cond
   ((null lista) '0)
   (t (+ (car lista) (Suma (cdr lista))))
  ))
































