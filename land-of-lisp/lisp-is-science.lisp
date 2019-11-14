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




