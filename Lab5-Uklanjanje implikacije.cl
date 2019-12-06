Teorijska podloga:

; 1) Primena rezolucije - Skupu aksioma i premisa se doda negacija teoreme, pa ako se dobije kontradikcija,
;    teorema je dokazana.
; 2) Literal je atomska formula ili njena negacija
; 3) Klauzula je skup literala koji predstavlja njihovu disjunkciju.

; Pravilo za prevodjenje:   a => b postaje -a v b 
;Dakle izraz tipa (if a b) je potrebno prevesti u u (or (not a) b)

(defun eliminisi (list)
  (cond ((or (null list) (atom list)) list) ; ako je prazno ili je atom, vrati listu instant
       ; ((equal 'not (car list)) (append (list(eliminisi (car list)) (eliminisi (cdr list))))) ; ovo sam probavao nesto samo
        ((equal 'if (car list)) (append (list 'or (list 'not (eliminisi (cadr list)))) (eliminisi (caddr list))))
        (:else (cons (eliminisi (car list)) (eliminisi (cdr list)))) ; ako ne naidje na if,vrati element samo i pozove za ostatak liste.
	)
)

;; Primer poziva funkcije : (eliminisi '(if (Pas Lesi) (Zivotinja Lesi)))
;; Jednostavniji : (eliminisi '(if a b))




;;;Drugi korak : suzavanje negacije, ima vise pravila:
(defun pravilo1 (x)
  (cond
   ((equal (car x) (cadr x)) (caddr x))
   (t (cons 'not (list x))))) 
;;primer (pravilo1 '(not not alfa)) ---> alfa

;drugo pravilo,primer poziva ; (pravilo2 '(not (and a b))) ---> (or (not a) (not b))
(defun pravilo2 (x)
  (cond
   ((equalp (car x) 'not)
    (if (equal (caar(cdr x)) 'and) (list 'or (list 'not (nth 1(nth 1 x))) (list 'not (nth 2(nth 1 x)))) (format t "~% nije moguce izvrsiti oper")))
   (t (cons (car x) (pravilo2 (cdr x))))
   )
  )
;;trece pravilo, primer poziva: (pravilo3 '(not (or a b))) ---> (and (not a) (not b))
(defun pravilo3 (x)
  (cond
   ((equalp (car x) 'not)
    (if (equal (caar(cdr x)) 'or) (list 'and (list 'not (nth 1(nth 1 x))) (list 'not (nth 2(nth 1 x)))) (format t "~% nije moguce izvrsiti oper")))
   (t (cons (car x) (pravilo2 (cdr x))))
   )
  )
;; za preostala 2 nisam siguran




;;;;;;Peti korak: Izbacivanje univerzalnog 

(defun izbaci_univerzalni (lista)
  (cond ((or (null lista) (atom lista)) lista)
        ((equal 'forall (car lista)) (izbaci_univerzalni (cddr lista)))
        (t (cons (izbaci_univerzalni (car lista)) (izbaci_univerzalni (cdr lista))))))

;;primer poziva: (and alfa  (or beta gama))  ->  {alfa}, {beta, gama} (skup klauzula)


;;Sedmi zadatak , Konjunkcija :

(defun konjunkcija (lista)
	(cond
		((null lista) '())
  ;; proveri da li je AND prvi clan i da li je u posle Alfa OR,ako jeste treba da pita da li je 3 clan
		((and (equal (car lista) 'and) (equal (caaddr lista) 'or))
   (if (listp  (nth 1 lista)) ;; ovde pita da li je na mestu posle AND lista ili ne
       ;;Ako je atom,onda njega samo ubaci u krajnju listu,a ako nije lista onda ce da kreira podlistu
		(list (cdr (cadr lista))  (list (cadr (caddr lista)) (caddr (caddr lista))) (list (car lista) (caaddr lista)))
		(list (list (cadr lista))  (list (cadr (caddr lista)) (caddr (caddr lista))) (list (car lista) (caaddr lista)) )))))
;;Primer poziva: (konjunkcija '(and alfa  (or beta gama))) ---> ((ALFA) (BETA GAMA) (AND OR))
;; kada je prvi deo listp=true : (konjunkcija '(and (not alfa) (or beta gama))) ---> ((ALFA) (BETA GAMA) (AND OR))
