
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

