;  1
(defun my-last (l) 
  (if (cdr l)
      (my-last (cdr l))
      (car l)))

;(print (my-last '(1 2 3 4 e)))
;(print (my-last '(1)))
;(print (my-last '()))
;(print (my-last '(1 2 nil)))

;  2
(defun my-but-last (l)
  (if (cddr l)
      (my-but-last (cdr l))
      l))

;(print (my-but-last '(1 2 3 a b)))
;(print (my-but-last '(a b)))
;(print (my-but-last '(b)))
;(print (my-but-last '(nil)))
;(print (my-but-last '()))

;  3
(defun my-element-at (l num)
  (if (equal (- num 1) 0)
      (car l)
      (my-element-at (cdr l) (- num 1))))

;(print (my-element-at '(a b c d e) 1))
;(print (my-element-at '(a b c d e) 3))
;(print (my-element-at '(a b c d e) 5))
;(print (my-element-at '(a) 1))
;(print (my-element-at '() 1))

;  4
(defun my-numlist (li num)
  (if  li
       (my-numlist (cdr li) (+ num 1))
       num))
(defun my-num-of-list (li) 
  (my-numlist li 0))

;(print (my-num-of-list '(5 4 3 2 1)))
;(print (my-num-of-list '(1 2 3 4 5 6 7 8 9 10)))
;(print (my-num-of-list '()))

;  5
(defun my-rev-list (li)
  (cons (cdr li)))

(defun my-rev-list(li)
  (my-rev-list-internal li nil))

(defun my-rev-list-internal (li rev)
  (if li
      (my-rev-list-internal (cdr li) (cons (car li) rev))
      rev))

;  6
(defun my-is-palindrome (li)
  (equal li (my-rev-list li)))

;(print (my-is-palindrome '(1 2 3 2 1)))
;(print (my-is-palindrome '(1 2 3 3 1)))
;(print (my-is-palindrome '(1 1)))
;(print (my-is-palindrome '(1)))
;(print (my-is-palindrome '()))

;  7
(defun my-flatten (li)
  (reduce #'append (mapcar (lambda (x)
                             (if (listp x)
                               (my-flatten x)
                               (list x)))
                           li)))

;(print (my-flatten '(1 2 3 4 5)))
;(print (my-flatten '(1 (2) 3 4 5)))
;(print (my-flatten '(1 (2 (3)) 4 5)))
;(print (my-flatten '(1 (((2) (3)) (4 5)))))
;(print (my-flatten '()))

;  8
(defun my-elm-dup (lst)
  (reduce #'append (maplist (lambda (li)
                              (if (eq (car li) (cadr li))
                                nil
                                (list (car li))))
                            lst)))

;(print (my-elm-dup '(a a a a b c c a a d e e e e)))
;(print (my-elm-dup '(a a a a)))
;(print (my-elm-dup '(1 2 2 1 1 2)))
;(print (my-elm-dup '()))

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

;(print (my-pack-dup '(a a a a b c c a a d e e e e)))
;(print (my-pack-dup '(a a a a)))
;(print (my-pack-dup '(1 2 2 1 1 2)))
;(print (my-pack-dup '()))

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

;(print (my-encode '(a a a a b c c a a d e e e e)))
;(print (my-encode '(a a a a)))
;(print (my-encode '(1 2 2 1 1 2)))
;(print (my-encode '()))

;  11

(defun my-encode-modified (lst)


;(print (my-encode-modified '(a a a a b c c a a d e e e e)))
;(print (my-encode-modified '(a a a a)))
;(print (my-encode-modified '(1 2 2 1 1 2)))
;(print (my-encode-modified '()))






