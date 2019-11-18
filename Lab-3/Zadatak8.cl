
(setq graf '((A (B C)) (B (A D E)) (C (A G F)) (D (B H)) (E (B I G)) (F (C J)) (G (E C J)) (H (D)) (I (E J)) (J (I G F))))

(defun uporedi (l1 l2) 
  (cond ((null l1) '())
        ((member (car l1) l2) (car l1))
        (t (uporedi (cdr l1) l2))))

(defun proveri(l1 l2)
  (cond 
   ((null l2) '())
   ((member l1 l2) l1)
   (t '())))

//vraca cvor gde se preklapaju
(defun zajednicki(ls1 ls2)
  (cond
   ((null ls1) NIL)
   ((null ls2) NIL)
   ((not (member (car ls1) ls2)) (get-intersect (cdr ls1) ls2))
   (:else (list (car ls1)))))

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

//obilazak dupli,2 starta 2 cilja i 2 niza obradjenih cvorova

(defun trazi (graf start1 cilj1 cvorovi1 start2 cilj2 cvorovi2)
  (cond ((uporedi start1 start2) (append cvorovi1 ( append (zajednicki start1 start2)(reverse cvorovi2))))
        (t (let* ((cvorovi11 (append cvorovi1 (list (car start1))))
                  (cvorovi22 (append cvorovi2 (list (car start2))))
                  (potomci11 (dodaj-potomke graf (car start1) (append start1 cvorovi11)))
                  (potomci22 (dodaj-potomke graf (car start2) (append start2 cvorovi22)))
                  (start11 (append (cdr start1) potomci11))
                  (start22 (append (cdr start2) potomci22))
                  (put (trazi graf start11 cilj1 cvorovi11 start22 cilj2 cvorovi22)))
             (cond ((null put) '())
                   (t put))))))

//poziv funkcije:
(trazi graf '(A) 'J '() '(J) 'A '()) 
; treba da vrati (A B E I J) za navedeni graf
//POKUSAJ na pravilan nacin:
//potrebna je funkcija koja ce ispitivati da li se odredjeni atom nalazi u listi

(defun provera (cvor lista)
  (cond
   ((null lista) '())
   ((member cvor lista) t)
   (t '())
   )
  )
(defun trazi (graf start1 cilj1 cvorovi1 start2 cilj2 cvorovi2)
  (cond
   ((provera (car start1) cvorovi2) (append cvorovi1 (reverse cvorovi2)))
   ((provera (car start2) cvorovi1) (append cvorovi1 (reverse cvorovi2)))
        (t (let* ((cvorovi11 (append cvorovi1 (list (car start1))))
                  (cvorovi22 (append cvorovi2 (list (car start2))))
                  (potomci11 (dodaj-potomke graf (car start1) (append start1 cvorovi11)))
                  (potomci22 (dodaj-potomke graf (car start2) (append start2 cvorovi22)))
                  (start11 (append (cdr start1) potomci11))
                  (start22 (append (cdr start2) potomci22))
                  (put (trazi graf start11 cilj1 cvorovi11 start22 cilj2 cvorovi22)))
             (cond ((null put) '())
                   (t put))))))

//poziv: (trazi graf '(A) 'J '() '(J) 'A '())


//Dodatna preciznost:

(defun trazi (graf start1 cilj1 cvorovi1 start2 cilj2 cvorovi2)
  (cond
   ((equalp (car start1) (car start2)) (append cvorovi1 (reverse (butlast cvorovi2))))
   ((provera (car start1) cvorovi2) (append cvorovi1 (reverse cvorovi2)))
   ((provera (car start2) cvorovi1) (append cvorovi1 (reverse cvorovi2)))
        (t (let* ((cvorovi11 (append cvorovi1 (list (car start1))))
                  (cvorovi22 (append cvorovi2 (list (car start2))))
                  (potomci11 (dodaj-potomke graf (car start1) (append start1 cvorovi11)))
                  (potomci22 (dodaj-potomke graf (car start2) (append start2 cvorovi22)))
                  (start11 (append (cdr start1) potomci11))
                  (start22 (append (cdr start2) potomci22))
                  (put (trazi graf start11 cilj1 cvorovi11 start22 cilj2 cvorovi22)))
             (cond ((null put) '())
                   (t put))))))