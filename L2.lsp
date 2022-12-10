;Scrieți o funcție care testează dacă un nod apartine unui n-arbore, definit ca:
;(rădăcină lista_noduri_subarbore_1 lista_noduri_subarbore_2 ... lista_noduri_subarbore_n)

;Exemplu: pentru arborele (a (b (c)) (d) (e (f))) şi nodul b => true

;varianta recursivă
(defun find_node_rec (l node)
    (cond
        ((null l) 
            nil
        )
        ((listp (car l)) 
            (cond
                ((equal (find_node_rec (car l) node) T)
                    T
                )
                (T 
                    (find_node_rec (cdr l) node)
                )
            )
        )
        ((equal (car l) node) 
            T
        )
        (T 
            (find_node_rec (cdr l) node)
        )
    )
)

(write (find_node_rec '(a (b (c)) (d) (e (f))) 'd))
(terpri)

;varianta cu mapcar
(defun atom_all(L)
    (cond
        ((atom L)
            (list L)
        )
        (T
            (apply 'append (mapcar 'atom_all L))
        )
    )
)

(defun find_node (L)
    (cond
        ( (atom L)
            (cond
                ( (eq L x)
                    1
                )
                (t 0)
            )
        )
        (t
            (mapcar 'find_node L)
        )
    )
)

(defun find_main (L node)
    (defvar x node)
    (apply 'max (atom_all (find_node L)))
)

(write (find_main '(a (b (c)) (d) (e (f))) 'd))
(terpri)
(terpri)


;Scrieți o funcție care șterge toate aparițiile unui atom de pe toate nivelurile unei liste neliniare.

;varianta recursivă
(defun remove_atom_rec (l atom)
    (cond
        ((null l) 
            ()
        )
        ((listp (car l))
            (cons (remove_atom_rec (car l) atom) (remove_atom_rec (cdr l) atom) )
        )
        ((equal (car l) atom) 
            (append () (remove_atom_rec (cdr l) atom ) ) )
        (T
            (cons (car l) (remove_atom_rec (cdr l) atom ) )
        )
    )
)

(write (remove_atom_rec '(a (b (c)) (d) (d (d e f))) 'd))
(terpri)

;varianta cu mapcar
(defun remove_atom (L)
    (cond
        ((atom L)
            (cond
                ((equal L x) nil)
                (t L)
            )
        )
        (t
            (mapcar 'remove_atom L)
        )
    )
)

(defun remove_main (L node)
    (defvar x node)
    (remove_atom L)
)

(write (remove_main '(a (b (c)) (d) (d (d e f) d (d)) d) 'd))
