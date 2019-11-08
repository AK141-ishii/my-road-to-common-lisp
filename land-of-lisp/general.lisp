;; ##########################################
;; # GlobalSettings / Utility
;; ##########################################
(setf *print-circle* t)
(defun mprint (target)
  (multiple-value-bind (a b) (eval target)
    (print a)
    (print b)))

;; ##########################################
;; LIST / CONSCELL / PAIR
;; ##########################################
(defun my-conscell-1 ()
  (cons 1 (cons 2 (cons 3 (cons 4 nil)))))
(defun my-dotlist-1 ()
  (cons 1 (cons 2 (cons 3 4))))
(defun my-pair-1 ()
  (cons 1 2))
(defun my-circle ()
  (let ((out '(1 2 3)))
    (setf (cdddr out) out)
    out))
  (list 1 2 3)
(defun my-alist()
  (let ((out '((b . double)
               (l . small)
               (j . med))))
    (push '(l . large) out)
    out))

;; ##########################################
;; # ARRAY
;; ##########################################
(defun my-array()
  (let ((out (make-array 3)))
    (setf (aref out 1) 'foo)
    out))
(defun my-foo-array()
  (let ((out (make-array 4)))
    (setf (aref out 2) (list 'x 'y 'z))
    (setf (car (aref out 2)) (make-hash-table))
    (setf (gethash 'zonik (car (aref out 2))) 5)
    out))
; Access to very long array takes shorter time than that to list...?
;  (defparameter very-long-y (loop repeat 5000000 collect 1))
;  (defparameter very-long-arr (make-array 5000000))

;; ##########################################
;; # HASH-TABLE
;; ##########################################
(defun my-hash()
  (let ((out (make-hash-table)))
    (setf (gethash 'bob out) 10)
    out))

;; ########################################
;; # STRUCT
;; ########################################
(defstruct person name age)
(defparameter *bob* (make-person :name "BOB" :age 27))
(defparameter *unknown* #S(person :name "unknown" :age 1))



;; ########################################
;; Generic Programming : Sequence
;; ########################################
;; OK
(length '(1 2 3 4 6))
(length (make-array 5))
(length "adskflj")
;; NG
; dotlist is not Sequence. 
;(length (cons 1 (cons 2 (cons 3 4))))
(find-if #'numberp '(a b c 4 e))
(count #\c "ab4cabc")
(position #\c "ab4cabc")
(some (lambda (x)
               (< x 5))
             '(1 2 3 4 5 6))
(every (lambda (x)
               (< x 5))
             '(1 2 3 4 5 6))

;; reduce : (reduce 'func 'target)
(reduce (lambda (best next)
          (if (and (evenp next) (> next best))
            next
            best))
        '(1 2 3 4 5 4 3 24)
        :initial-value 0)

;; map : (map 'ret-type 'func 'target)
(map 'list
     (lambda (x)
       (if (eq x #\s)
         #\S
         x))
     "This is a test")

;; other : subseq / sort
(subseq "america" 2 6)
(sort '(1 1 8 4 6 8 2 1 8 3 5 8 2 3  9 9) #'<)

;; TYPE DISPATCHING
(defmethod add ((a number) (b number))
  (+ a b))
(defmethod add ((a list) (b list))
  (append a b))

