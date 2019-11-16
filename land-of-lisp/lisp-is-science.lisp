;; ########################################
;; Lisp Is Science
;; : return the same if args are same (referential transparetn)
;; : no side-effect, just return values. 
;; ########################################

; CLEAN / FUNCTIONAL
(sin 0.5)
(defun add-widget (database widget)
  (cons widget database))

; DIRTY / IMPERATIVE
(defparameter *database* nil)
(defun main-loop ()
  (loop (princ "Please enter the name of a new widget:")
        (setf *database* (add-widget *database* (read)))
        (format t "The database contains the following: ~a~%" *database*)))


; ########################################
; Optimized Common Lisp
; ########################################

; Closure / Lexical
(defparameter *newfoo* (lambda () (let ((x 5)) x)))
(funcall *newfoo*) ; 5 (x is alive)
(let ((line-number 0))
  (defun my-print (x)
    (print line-number)
    (print x)
    (incf line-number)
    nil))
(my-print "this") ; line-number will increase

; Memo
;  Once called, it will store the relationships between args->results.
(defun foo (lst)
  (length lst))

(let ((old-foo (symbol-function 'foo))
      (previous (make-hash-table)))
  (defun foo (arg)
    (or (gethash arg previous)
        (setf (gethash arg previous) (funcall old-foo arg)))))

; Tail Recursion
(defparameter *biglist* (loop for i below 100000 collect 'x))
(defun my-poor-length (lst)
  (if lst
      (1+ (my-poor-length (cdr lst)))
      0))
; (my-poor-length *biglist*) -> Takes too much time !
(defun my-clever-length (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f lst 0)))
(my-clever-length *biglist*) ; -> quick !

; ########################################
; Macro
; ########################################

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(let1 foo (+ 2 3)
  (* foo foo))

(defun add (a b)
  (let1 x (+ a b)
    (format t "The sum is ~a" x)
    x))

(macroexpand '(let1 foo (+ 2 3)
                (* foo foo)))


; defun my-length using defmacro
; here is my-length without macro
(defun my-length (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f lst 0)))
(my-length '(1 2 3))

; 1st version of split macro (containing bug)
(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))

(defun my-length (lst)
  (labels ((f (lst acc)
             (split lst
                    (f tail (1+ acc))
                    acc)))
    (f lst 0)))

; anti-split call of macro
(split (progn (princ "lispppp")
              '(2 3))
       (format t "~a ~a ~%" head tail)
       (format t "no"))
; inspection
(macroexpand '(split (progn (princ "lispppp")
              '(2 3))
       (format t "~a ~a ~%" head tail)
       (format t "no")))

; 2nd version of split macro (still containing bug)
(defmacro split (val yes no)
  `(let1 x ,val
    (if x
        (let ((head (car x))
              (tail (cdr x)))
          ,yes)
        ,no)))
; anti-split call of macro
;(let1 x 100
;  (split '(2 3)
;         (+ x head)
;         nil))
; inspection
(macroexpand '(split '(2 3)
                     (+ x head)
                     nil))

; final version of split macro
(defmacro split (val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))
; inspection
(macroexpand '(split '(2 3)
                     (+ x head)
                     nil))

; recursion macro
(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))
(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels ((self ,(mapcar #'car p)
                ,@body))
       (self ,@(mapcar #'cdr p)))))
; test
(recurse (n 9)
  (fresh-line)
  (if (zerop n)
      (princ "lift-off!")
      (progn (princ n)
             (self (1- n)))))

; my-length with all macros
(defun my-length (lst)
  (recurse (lst lst
                acc 0)
    (split lst
           (self tail (1+ acc))
           acc)))
(my-length '(1 2 3 4 5))

; actually, my-length can be written with reduce smartly
(defun my-length (lst)
  (reduce (lambda (x i)
            (1+ x))
          lst
          :initial-value 0))

; ########################################
; DSL : Domain Specific Language
; ########################################







