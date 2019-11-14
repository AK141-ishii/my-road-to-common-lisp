;  1
(defun my-last (l) 
  (if (cdr l)
      (my-last (cdr l))
      (car l)))

(my-last 
  '(1 2 3 4 e))
(my-last
  '(1))
(my-last
  '())
(my-last
  '(1 2 nil))

;  2
(defun my-but-last (l)
  (if (cddr l)
      (my-but-last (cdr l))
      l))

(my-but-last
  '(1 2 3 a b))
(my-but-last
  '(a b))
(my-but-last
  '(b))
(my-but-last
  '(nil))
(my-but-last
  '())

;  3
(defun my-element-at (l num)
  (if (equal (- num 1) 0)
      (car l)
      (my-element-at (cdr l) (- num 1))))

(my-element-at 
  '(a b c d e)
  1)
(my-element-at
  '(a b c d e)
  3)
(my-element-at
  '(a b c d e)
  5)
(my-element-at
  '(a)
  1)
(my-element-at
  '()
  1)

;  4
(defun my-num-of-list (li) 
  (reduce 
    #'+
    (mapcar (lambda (val) 1)
            li)))

(my-num-of-list 
  '(5 4 3 2 1))
(my-num-of-list
  '(1 2 3 4 5 6 7 8 9 10))
(my-num-of-list
  '(a b c d e))
(my-num-of-list
  '())

;  5
(defun my-rev-list(li)
  (reduce (lambda (prev next)
            (cons next prev)
            )
          li
          :initial-value nil
          )
  )

(my-rev-list 
  '(1 2 3 4 5))
(my-rev-list 
  '(a b c d e))
(my-rev-list 
  '(a b c d e))
(my-rev-list 
  '(1))
(my-rev-list 
  '())

;  6
(defun my-is-palindrome (li)
  (equal li (my-rev-list li)))

(my-is-palindrome '(1 2 3 2 1))
(my-is-palindrome '(1 2 3 3 1))
(my-is-palindrome '(1 1))
(my-is-palindrome '(1))
(my-is-palindrome '())

;  7
(defun my-flatten (li)
  (reduce #'append (mapcar (lambda (x)
                             (if (listp x)
                               (my-flatten x)
                               (list x)))
                           li)))

(my-flatten '(1 2 3 4 5))
(my-flatten '(1 (2) 3 4 5))
(my-flatten '(1 (2 (3)) 4 5))
(my-flatten '(1 (((2) (3)) (4 5))))
(my-flatten '())

;  8
(defun my-elm-dup (lst)
  (reduce #'append (maplist (lambda (li)
                              (if (eq (car li) (cadr li))
                                nil
                                (list (car li))))
                            lst)))

(my-elm-dup '(a a a a b c c a a d e e e e))
(my-elm-dup '(a a a a))
(my-elm-dup '(1 2 2 1 1 2))
(my-elm-dup '())

;  9
(defun my-pack-dup (lst)
  (if lst
    (reverse (reduce (lambda (prev next)
                       (if (equal (caar prev) next)
                         (cons (cons next (car prev)) (cdr prev))
                         (cons (list next) prev)))
                     (cdr lst)
                     :initial-value (list (list(car lst)))))
  '()))

(my-pack-dup '(a a a a b c c a a d e e e e))
(my-pack-dup '(a a a a))
(my-pack-dup '(1 2 2 1 1 2))
(my-pack-dup '())

;  10
(defun my-encode (lst)
  (if lst
    (reverse (reduce (lambda (prev next)
                       (if (equal (cadar prev) next)
                         (cons (list (incf (caar prev)) (cadar prev)) (cdr prev))
                         (cons (list 1 next) prev)))
                     (cdr lst)
                     :initial-value (list (list 1 (car lst)))))
    '()))

(my-encode '(a a a a b c c a a d e e e e))
(my-encode '(a a a a))
(my-encode '(1 2 2 1 1 2)) 
(my-encode '()) 

;  11
(defun my-encode-modified (lst)
  (if lst
      (reverse (reduce (lambda (prev next)
                         (if (listp (car prev))
                             (if (equal (cadar prev) next)
                                 (cons (list (incf (caar prev))
                                             (cadar prev))
                                       (cdr prev))
                                 (cons next prev))
                             (if (equal (car prev) next)
                                 (cons (list 2
                                             (car prev))
                                       (cdr prev))
                                 (cons next prev))))
                       (cdr lst)
                       :initial-value (list (car lst))))
      nil))


(my-encode-modified '(a a a a b c c a a d e e e e)) ; ((4 A) B (2 C) ...
(my-encode-modified '(a a a a e))
(my-encode-modified '(1 2 2 1 1 2))
(my-encode-modified '())

;  12
(defun my-decode-modified (lst)
   (mapcan (lambda (next)
              (labels ((f (lst num sym)
                         (if (zerop num)
                             lst
                             (f (cons sym lst) (1- num) sym))))
                
              (if (listp next)
                  (f (list) (cadr next) (car next))  
                  (list next ))))
            lst))
(my-decode-modified '((A 4) B (C 2) (A 2) D (E 4)))
(my-decode-modified '((A 5)))
(my-decode-modified '(1 (2 2) (1 2) 2))
(my-decode-modified 'nil)

;  13
;  skip it

;  14
(defun my-dupli (lst)
  (reverse
    (reduce (lambda (prev nx)
              (cons nx (cons nx prev)))
            lst
            :initial-value (list))))
(my-dupli '(a b c c d)) ;(A A B B C C C C D D)
(my-dupli '(1 2 3 4 5)) ;(1 1 2 2 3 3 4 4 5 5)
(my-dupli '()) ;NIL

;  15
(defun my-repli (lst cnt)
  (apply #'append (mapcar (lambda (elm)
                            (labels ((f (st elm cnt)
                                       (if (zerop cnt)
                                           st
                                           (f (cons elm st) elm (1- cnt)))))
                              (f (list) elm cnt)))
                          lst)))

(my-repli '(a b c) 3) ;(A A A B B B C C C)
(my-repli '(1 2 3) 4) ;(1 1 1 1 2 2 2 2 3 3 3 3)

;  16
(defun my-drop (lst cnt)
  (labels ((f (out cur-cnt st)
             (if (null st)
                 out
                 (if (zerop cur-cnt)
                     (f out (1- cnt) (cdr st))
                     (f (cons (car st) out) (1- cur-cnt) (cdr st))))))
    (reverse (f (list) (1- cnt) lst))))



(my-drop '(a b c d e f g h i k) 3) ;(A B D E G H K)
(my-drop '(1 2 3 4 5 6 7 8 9 0) 3) ;(1 2 4 5 7 8 0)
(my-drop '(a b c d e f g h i k) 2) ;(A C E G I)
(my-drop '(a b c d e f g h i k) 1) ;(A C E G I)

;  17
(defun my-split (lst cnt)
  (labels ((f (out cnt lst)
             (if (zerop cnt)
                 (list (reverse out) lst)
                 (f (cons (car lst) out)
                    (1- cnt)
                    (cdr lst)))))
    (f (list) cnt lst)))
(my-split '(a b c d e f g h i k) 1) ;((A) (B C D E F G H I K))
(my-split '(a b c d e f g h i k) 3) ;((A B C) (D E F G H I K))
(my-split '(a b c d e f g h i k) 5) ;((A B C D E) (F G H I K))

;  18
(defun my-slice (lst stt end)
  (labels ((f (out scnt ecnt lst)
             (if (zerop scnt)
                 (if (zerop ecnt)
                     (reverse out)
                     (f (cons (car lst) out)
                        scnt
                        (1- ecnt)
                        (cdr lst)))
                 (f out (1- scnt) (1- ecnt) (cdr lst)))))
    (f (list) (1- stt) end lst)))

(my-slice '(a b c d e f g h i k) 3 7) ;(C D E F G)
(my-slice '(a b c d e f g h i k) 1 5) ;(A B C D E)

;  19
(defun my-rotate (lst cnt)
  (labels ((f (lst cnt)
             (if (zerop cnt)
                 lst
                 (f (reverse (cons (car lst)(reverse (cdr lst))))
                    (1- cnt)))))
    (if (< cnt 0)
        (f lst (+ (length lst) cnt))
        (f lst cnt))))

(my-rotate '(a b c d e f g h) 3) ;(D E F G H A B C)
(my-rotate '(a b c d e f g h) -2) ;(G H A B C D E F)

;  20
(defun my-remove-at (lst plc))

(my-remove-at '(a b c d) 2) ;(A C D)





















