

(setf *print-circle* t)

(defun mprint (target)
  (multiple-value-bind (a b) (eval target)
    (print a)
    (print b)))

;; LIST / CONSCELL / PAIR

(defun my-conscell-1 ()
  (cons 1 (cons 2 (cons 3 (cons 4 nil)))))

(defun my-dotlist-1 ()
  (cons 1 (cons 2 (cons 3 4))))

(defun my-pair-1 ()
  (cons 1 2))

(defparameter my-circle (list 1 2 3))
(setf (cdddr my-circle) my-circle)

(defparameter my-alist '((b . double)
                         (l . small)
                         (j . med)))
(push '(l . large) my-alist)

;; ##########################################
;; # ARRAY
;; ##########################################

(defparameter x (make-array 3))
(setf (aref x 1) 'foo)
(setf foo (make-array 4))
(setf (aref foo 2) (list 'x 'y 'z))
(setf (car (aref foo 2)) (make-hash-table))
(setf (gethash 'zonik (car (aref foo 2))) 5)
; Access to very long array takes shorter time than that to list...?
;(defparameter very-long-y (loop repeat 5000000 collect 1))
;(defparameter very-long-arr (make-array 5000000))


;; ##########################################
;; # HASH-TABLE
;; ##########################################

(defparameter my-hash (make-hash-table))
(setf (gethash 'bob my-hash) 10)


;; ########################################
;; # STRUCT
;; ########################################
(defstruct person name age)
(defparameter *bob* (make-person :name "BOB" :age 27))
(defparameter *unknown* #S(person :name "unknown" :age 1))


;; ########################################
;; Generic Programming
;; ########################################






















