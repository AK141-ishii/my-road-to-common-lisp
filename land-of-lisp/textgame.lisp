;; Global variables

(defparameter *nodes* '(
    (living (You are in the living. here is a wizard.))
    (garden (You are in the garden. here is a well.))
    (attic  (You are in the attic. here is a torch.))
))

(defparameter *edges* `((living (garden west door)
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
            `(Here is ,obj \.)))
    (apply #'append (mapcar #'describe-obj (objectsAt loc objs obj-loc)))))


;; Action commands

(defun look()
    (append (describeLocation *location* *nodes*)
            (describePaths *location* *edges*)
            (describeObjects *location* *objects* *objects-location*)))

(defun walk(direction)
(let 
    ((next (find direction
                 (cdr (assoc *location* *edges*))
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



(game-repl)








