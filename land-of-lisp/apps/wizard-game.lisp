; #########################################################################
; - Wizard Game -
;  Text game
; #########################################################################

;; Global variables

(defparameter *wiz-nodes* '(
    (living (You are in the living. here is a wizard.))
    (garden (You are in the garden. here is a well.))
    (attic  (You are in the attic. here is a torch.))
))

(defparameter *wiz-edges* `((living (garden west door)
                                (attic  upstairs ladder))
                        (garden (living east door))
                        (attic  (living downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *objects-location* '((whiskey living)
                                   (bucket  living)
                                   (chain   garden)
                                   (frog    garden)))

(defparameter *allowed-commands* '(look walk pickup inventry))

;; Player's Status

(defparameter *location* 'living)

;; ------------ Basic Function ------------

(defun describeLocation (location nodes)
    (cadr (assoc location nodes)))

(defun describePath (edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.)) 

(defun describePaths (location edges)
    (apply #'append (mapcar #'describePath (cdr (assoc location edges)))))

(defun objectsAt (loc objs obj-locs)
    (labels ((at-loc-p (obj)
            (eq (cadr (assoc obj obj-locs)) loc)))
            (remove-if-not #'at-loc-p objs)))

(defun describeObjects (loc objs obj-loc)
    (labels 
        ((describe-obj (obj)
            `(Here is ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objectsAt loc objs obj-loc)))))


;; Action commands

(defun look()
    (append (describeLocation *location* *wiz-nodes*)
            (describePaths *location* *wiz-edges*)
            (describeObjects *location* *objects* *objects-location*)))

(defun walk(direction)
(let 
    ((next (find direction
                 (cdr (assoc *location* *wiz-edges*))
                 :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))

(defun pickup(obj)
    (cond 
        ((member obj (objectsAt *location* *objects* *objects-location*))
           (push (list obj 'body) *objects-location*)
          `(carrying the ,obj))
        (t 
          '(you cannot get that.))))

(defun inventry()
    (cons 'item- (objectsAt 'body *objects* *objects-location*)))

;; Interface

(defun greeting () 
    (print "put your name.")
    (let ((name (read-line)))
         (princ "Hello, ")
         (princ name)
         (princ " !")))

(defun addFive () 
    (print "Give me a number:")
    (let ((num(read)))
         (print (+ num 5))))

;; GameREPL

(defun game-read()
    (let ((cmd (read-from-string
                (concatenate 'string "(" (read-line) ")" ))))
         (flet ((quote-it (x)
                (list 'quote x)))
                (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-eval(sexp)
(if (member (car sexp) *allowed-commands*)
    (eval sexp)
    '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
      (rest (cdr lst)))
        (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
              ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
              ((eql item #\") (tweak-text rest caps (not lit)))
              (lit (cons item (tweak-text rest nil lit)))
              (caps (cons (char-upcase item) (tweak-text rest nil lit)))
              (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print(lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                             'list)
                 t
                 nil)
         'string))
  (fresh-line))

(defun game-repl()
    (let ((cmd (game-read)))
         (unless (eq (car cmd) 'quit)
                 (game-print (game-eval cmd))
                 (game-repl))))

;; ########### NOW, I HAVE THE POWER OF MACROS ########### 

(defun have (object)
  (member object (cdr (inventry))))

(defmacro game-action (command subj obj place &body body)
  (let ((subject (gensym))
        (object (gensym)))
  `(progn (defun ,command (,subject ,object)
            (if (and (eq *location* ,place)
                     (eq ,subject ,subj)
                     (eq ,object ,obj)
                     (have ',subj))
                ,@body
                '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))
  )

(defparameter *chain-welded* nil)
(game-action weld chain bucket attic
             (if (and (have 'bucket) (not *chain-welded*))
                 (progn (setf *chain-welded* 't)
                        '(the cahin is now securely selded to the bucket.))
                 '(you do not have a bucket)))

(defparameter *bucket-filled* nil)
(game-action dunk bucket well garden
             (if *chain-welded*
                 (progn (setf *bucket-filled* 't)
                        '(the bucket is now full of water.))
                 '(the water level is too low to reach.)))

(game-action splash bucket wizard living
             (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
                   ((have 'frog) '(the wizard awakens and sees that you stole his frog.
                                       he is so upset he banishes you to the
                                       netherworlds- you lose! the end.))
                   (t '(the wizard awakens from his slumber and greets you warmly
                            he hands you the magic low-carb donut- you win! the end.))))

;(game-repl)
