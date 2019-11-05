(defun my-last (l) 
  (if (cdr l)
      (my-last (cdr l))
      (car l)))

(defun my-but-last (l)
  (if (cddr l)
      (my-but-last (cdr l))
      l))

(defun element-at (l num)
  (if (equal (- num 1) 0)
      (car l)
      (element-at (cdr l) (- num 1))))


(defun my-numlist (li num)
  (if  li
       (my-numlist (cdr li) (+ num 1))
       num
       ))

(defun my-num-of-list (li) 
  (my-numlist li 0))

(defun my-rev-list (li)
  (cons (cdr li)))

(defun my-rev-list(li)
  (my-rev-list-internal li nil))

(defun my-rev-list-internal (li rev)
  (if li
      (my-rev-list-internal (cdr li) (cons (car li) rev))
      rev))

(defun my-is-palindrome (li)
  (equal li (my-rev-list li)))

(defun my-flatten (li)

(defun my-flatten-internal (li res)







