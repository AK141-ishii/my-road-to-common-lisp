
; Utility###################

(defmacro dm (&body body)
  `(defmacro ,@body))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

; #########################

; Text ###################

(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var) ,@body)
           ,list)
     (let ((,var nil))
       ,result)))
(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

; #########################

; Prac

; (memq x choices)
; ->
; (member x choises :test #'eq)
(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

; (while rule ...bodys)
; ->
; (do ()
;   ((not rule))
;   ...bodys)
(defmacro while (rule &body body)
  `(do ()
       ((not ,rule))
       ,@body))

; (do ((w 3)
;      (x 1 (1+ x))
;      (y 2 (1+ y))
;      (z))
;   ((> x 10) (princ z) y)
;   (princ x)
;   (princ y))
; -> 
; (prog ((w 3) (x 1) (y 2) (z nil))
;       foo
;       (if (> x 10)
;           (return (progn (princ z) y)))
;       (princ x)
;       (princ y)
;       (psetq x (1+ x) y (1+ y))
;       (go foo))

; TODO
;(defmacro our-do (bindforms (test &rest res) &body body)
;  `(prog ,()
;     
;     )
;  
;  )

(defun initbind (bindforms)
  (mapcar (lambda (a)
            (if (null (cdr a))
                (list (car a) nil)
                (list (car a) (cadr a))))
          bindforms))

; TODO
;(defun stepforms (bindforms)
;  (mapcan (lambda (a)
;            (if ())
;            ))
;  )

; ########## MACRO EXCERCISE ###########

; #1 When
;(my-when c (&body body)) ->
;(if c
;    (&body)
;    nil)

;(my-when t
;  (print 1)
;  (print 1))
(dm my-when (c &body body)
    `(if ,c
         (progn ,@body)
         nil))


; #2 Progn
;(my-progn (&body body))
;(funcall (lambda () body))

;(progn
;  (print 1)
;  (print 1))
(dm my-progn (&body body)
          `(funcall (lambda () ,@body)))


; #3 Cond
;(my-cond (&body body)) ->
;(if (car b1) (cdr b1)
;    (if (car b2) (cdr b2)... nil))
(dm my-cond (&body body)
         (reduce
           (lambda (prev nxt)
                   `(if ,(car nxt)
                        (progn ,@(cdr nxt))
                        ,prev))
           (reverse body)
           :initial-value `()))


; #4 Let
; (my-let (bindings &body body))
; (funcall #'(lambda (b1 b2)
;              ,@body)
;   b1
;   b2)

;test
;(my-let ((x 1)
;         (y 2))
;  (+ x y))
(dm my-let (bindings &body body)
  `(funcall #'(lambda 
              ,(mapcar
                 (lambda (c)
                   (car c))
                 bindings)
              ,@body)
    ,@(mapcar
       (lambda (bind)
         `(progn ,@(cdr bind)))
       bindings)))

  



    





