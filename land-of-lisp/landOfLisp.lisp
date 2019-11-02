(defparameter *nodes* '(
    (living (here is a wizard.))
    (garden (here is a well.))
    (attic  (here is a torch.))
))

(defparameter *edges* `(
    (living (garden west door)
            (attic  upstairs ladder))
    (garden (living east door))
    (attic  (living downstairs ladder))
))

(defparameter *objects* '(whiskey bucket frog chain))
(defparameter *objects-location* '(
    (whiskey living)
    (bucket  living)
    (chain   garden)
    (frog    garden)
))

(defun describe-location (location nodes)
    (cadr (assoc location nodes)))

(defun describe-path (edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.)
)

(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges))))
)

(defun objects-at (loc objs obj-locs)
    (labels ((at-loc-p (obj)
            (eq (cadr (assoc obj obj-locs)) loc)))
            (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
    (labels 
        ((describe-obj (obj)
            `(you see ,obj on the floor.)
        ))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))
    )
)

;;; player's state

(defparameter *location* 'living-room)

(defun look()
    (append (describe-location *location* *nodes*)
            (describe-paths *location* *edges*)
            (describe-objects *location* *objects* *objects-location*))
)

(defun walk(direction)
(let 
    ((next (find direction
                 (cdr (assoc *location* *edges*))
                 :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.)
    )
)
)

(defun pickup(obj)
    (cond 
        ((member obj (objects-at *location* *objects* *objects-location*))
           (push (list obj 'body) *objects-location*)
          `(you are now carrying the ,obj))
        (t 
          '(you cannot get that.))
    )
)

(defun inventry()
    (cons 'item- (objects-at 'body *objects* *objects-location*))
)

;;(princ (walk 'west))
;;(princ (walk 'east))
;;(princ (pickup 'whiskey))
;;(princ (inventry))

(defun greeting () 
    (print "put your name.")
    (let
        ((name (read)))
        (print  `(hello ,name !))
    )
)

(defun add-five()
    (print "give me a number : ")
    (let ((name (read)))
         (print (+ name 5))
    )
)

(defun game-repl ()
    (loop (princ (eval (read))))
)







