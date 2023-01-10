; Fieraru Cristian-George
; 10LF202

; Ex9. Transformați un arbore de tipul (1) într-un arbore de tipul (2).

; left-subtree(L, nr) =
; { L, dacă L este atom
; { L1.(left-subtree(L2..Ln, nr)), dacă L1 este număr
; { (L1, 0), dacă nr = 1 şi L2 = 0
; { L1.(left-subtree(L2...Ln, L2 + nr - 1)), altfel

(defun left-subtree (l nr)
    (cond
        ( (atom l) 
            l
        )
        ( ( numberp (car l)) 
            (cons (car l) (left-subtree (cdr l) nr))
        )
        ( (and (= nr 1) (= (cadr l) 0)) 
            (list (car l) 0)
        ) 
        ( T 
            (cons (car l) (left-subtree (cdr l) (+ (cadr l) (- nr 1))))
        )
    )
) 

; right-subtree(L, nr) =
; { left-subtree(L, nr), dacă nr = 1
; { L, dacă L este atom
; { right-subtree(L3...Ln, L2 + nr - 1), altfel

(defun right-subtree (l nr)
    (cond 
        ( (= nr 1) 
            (left-subtree l nr)
        )
        ( (atom l) 
            l
        )
        ( T 
            (right-subtree (cddr l) (+ (cadr l) (- nr 1)))
        )
    )
)

; transform(L) = 
; { nil, dacă L = ()
; { L1.nil, dacă L2 = 0
; { (L1, transform(left-subtree(L3...Ln), 1)), dacă L2 = 1
; { (L1, transform(l-subtree(L3...Ln), 1), transform(r-subtree(L3...Ln, 2))), altfel

(defun transform (l)
    (cond 
        ( (null l)
            nil
        )
        ( (= 0 (cadr l)) 
            (cons (car l) nil)
        )
        ( (= 1 (cadr l)) 
            (list (car l) (transform (left-subtree (cddr l) '1)))
        )
        ( (= 2 (cadr l)) 
            (list (car l)
            (transform (left-subtree (cddr l) '1))
            (transform (right-subtree (cddr l) '2)))
        )
    )
)

(defvar l '(A 2 B 1 D 2 G 0 H 0 C 2 E 0 F 0))
(write (transform l))
(terpri)


; Ex3. Returnați numărul de niveluri dintr-un arbore de tipul (1).

; depth (L) = 
; { 0, dacă L2 este null
; { 1 + max( depth(L2),...,depth(Ln) ), altfel
    
(defun depth (tree)
    (cond
        ( (null (cdr tree))
            0
        )
        ( T
            (+ 1 (apply 'max (mapcar 'depth (cdr tree))))
        )
    )
)

(defvar tree (transform '(A 2 B 1 D 2 G 0 H 0 C 2 E 0 F 0) ))
(write (depth tree))