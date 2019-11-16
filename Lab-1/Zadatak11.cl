11. Napisati funkciju koja prevodi zadati broj iz dekadnog u binarni brojni sistem, tako da je svaka
cifra u binarnom sistemu element izlazne liste.
Zaglavlje funkcije: (prevedi broj)
Primeri poziva: (prevedi ’25) => (1 1 0 0 1)
NAPOMENA: Za celobrojno deljenje koristiti funkciju: (defun div (x y) (/ (- x (mod x y) y))).

(defun binary-list (n)
  (cond ((= n 0) (list 0))
        ((= n 1) (list 1))
        (t (nconc (binary-list (truncate n 2)) (list (mod n 2))))))