11.Definisati funkcije:
a)
palindrom, koja formira listu koja predstavlja palindrom od zadate re?i. Re?
je zadata kao lista slova. Nije dozvoljeno koristiti funkciju reverse.
Primeri:
(palindrom '(a n)) -> (a n a)
(palindrom '(a n a v o l i m)) -> (a n a v o l i m i l o v a n a)

Resenje:

(defun palindrom (list)
  (cond ((equal (length list) 1) list) // kada dodje na sredinu
        (:else (cons (car list) (append (palindrom (cdr list)) (list (car list)))))))
		

//sa rekurzijom:
(defun palindrome (l)
    (cond
        ( (null l) nil)
             (T (append l(reverseString (cdr l)) (list (car l))))
    )
)
//SA AND OPERACIJOM:
(defun brojevip (lista)
               (cond
                ((null lista) t) 
                ((atom (car lista)) (AND (numberp (car lista)) (brojevip (cdr lista))))
                (t (and (brojevip (car lista)) (brojevip (cdr lista))))))


b)
brojevip, koja ispituje da li su svi elementi prave liste numeri?ke vrednosti.
Primeri:
(brojevip '(1 2 3 a 4 5)) -> ()
(brojevip '((1 2) 3 ((4 (1 2) a) 6 b))) -> ()
(brojevip '((1 2) 3 ((4 (1 2) 5) 6 7))) -> T

Resenje:

(defun brojevip (lista)
               (cond
                ((null lista) t) 
                ((atom (car lista)) (if (numberp (car lista)) (brojevip (cdr lista)) '()))  
				                (t (and (brojevip (car lista)) (brojevip (cdr lista)))))) 