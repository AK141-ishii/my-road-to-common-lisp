
; Utility###################

(defmacro dm (&body body)
  `(defmacro ,@body))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

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



