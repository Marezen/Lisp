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

