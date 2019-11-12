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
; 
; ########################################
