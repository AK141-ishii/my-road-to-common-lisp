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
                       (if (equal (caar prev) next)
                         (cons (list (caar prev) (incf (cadar prev))) (cdr prev))
                         (cons (list next 1) prev)))
                     (cdr lst)
                     :initial-value (list (list (car lst) 1))))
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
                             (if (equal (caar prev) next)
                                 (cons (list (caar prev)
                                             (incf (cadar prev))) 
                                       (cdr prev))
                                 (cons next prev)
                                 )
                             (if (equal (car prev) next)
                                 (cons (list (car prev)
                                             2)
                                       (cdr prev))
                                 (cons next prev)
                                 )
                             )
                         )
                       (cdr lst)
                       :initial-value (list (car lst))
                       )
               )
      nil)
  )


(my-encode-modified '(a a a a b c c a a d e e e e))
(my-encode-modified '(a a a a e))
(my-encode-modified '(1 2 2 1 1 2))
(my-encode-modified '())

;  12
(defun my-decode-modified (lst))

(my-decode-modified '((A 4) B (C 2) (A 2) D (E 4)))
(my-decode-modified '((A 5)))
(my-decode-modified '(1 (2 2) (1 2) 2))
(my-decode-modified 'nil)





