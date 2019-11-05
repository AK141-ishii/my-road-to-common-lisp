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











