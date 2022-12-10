; Scrieți o funcție care să returneze suma tuturor elementelor numerice de la nivel superior.
; sum(lista) =
; { 0, dacă lista = []
; { L1 + sum(L2...Ln), dacă L1 este număr
; { sum(L2...Ln), altfel

(defun sum (l)
    (cond
        ((null l) 
            0
        )
        ((numberp (car l))
            (+ (car l) (sum (cdr l)))
        )
        (t
            (sum (cdr l))
        )
    )
)

;(write(sum '(1 2 3 (4 5 (6 7)) 8 9 (10 11)) ))


; Scrieți o funcție care să interclaseze 2 liste liniare sortate și care păstrează valorile duplicate.
; interclasare(X, Y) =
; { X, dacă Y = []
; { Y, dacă X = []
; { X1 U interclasare (X2...Xn, Y), dacă X1 < Y1
; { Y1 U interclasare (X, Y2...Yn), dacă X1 > Y1
; { X1 U Y1 U interclasare(X2...Xn, Y2...Yn), altfel

(defun interclasare (l1 l2)
    (cond
        ( (null l2) l1 )
        ( (null l1) l2 )
        ((< (car l1) (car l2)) (cons (car l1) (interclasare (cdr l1) l2)))
        ((> (car l1) (car l2)) (cons (car l2) (interclasare l1 (cdr l2))))
        (t (cons (car l1) (cons (car l2) (interclasare (cdr l1) (cdr l2)))))
    )
)

;(write(interclasare '(1 2 3 3 3 4) '(2 3 4 5 6)))


; Scrieți o funcție care să înlocuiască într-o listă neliniară L1 un element E cu o altă listă L2.
; replace_element(L, Elem, X) =
; { [], dacă L = []
; { replace_element(L1, Elem, X) U L2...Ln, dacă L1 este listă
; { X U replace_element(L2...Ln, Elem, X), dacă L1 = Elem
; { L1 U replace_element(L2...Ln, Elem, X), altfel

(defun replace_element (l1 element l2)
    (cond
        ((null l1) 
            ()
        )
        ((listp (car l1))
            (cons (replace_element (car l1) element l2) (cdr l1) )
        )
        ((equal (car l1) element) 
            (cons l2 (replace_element (cdr l1) element l2 ) ) )
        (t
            (cons (car l1) (replace_element (cdr l1) element l2 ) )
        )
    )
)

;(write(replace_element '(4 1 4 (4 5 6 (4 5 6))) '4 '(11 11)))


; Scrieți o funcție care să determine suma a două numere dintr-o reprezentate ca liste (fără a transforma listele in număr).
; sum(X, Y, transport) =
; { [], dacă X = [] şi transport = 0
; { [transport], dacă X = [] şi transport /= 0
; { sum(X, [0], transport), dacă Y = []
; { [(X1 + X2 + transport) mod 10] U sum(X2...Xn, Y2...Yn, (X1 + X2 + transport) div 10), altfel

(defun sum_helper (l1 l2 carry)
    (cond
        ((null l1)
            (cond
                ((zerop carry) ())
                (t (cons carry l1 ))
            )
        )
        ((null l2)
            (setq l2 '( 0 ))
            (sum_helper l1 l2 carry)
        )
        (t
            (setq sum (+ (+ (car l1) (car l2) carry) ) )
            (setq carry (truncate sum 10) )
            (cons (mod sum 10) (sum_helper (cdr l1) (cdr l2) carry ) )
        )
    )
)

(defun sum (l1 l2)
    (setq l1 (reverse l1) )
    (setq l2 (reverse l2) )
    
    (cond
        ((>= (length l1) (length l2))
            (reverse (sum_helper l1 l2 '0))
        )
        (t
            (reverse (sum_helper l2 l1 '0))
        )
    )
)

(write(sum '(9 9 0 5) '( 1 0 0 )))