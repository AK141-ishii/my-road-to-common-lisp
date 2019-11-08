(defparameter *max-level-length* 30)

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defun dot-label (exp)
  (if exp
    (let ((s (write-to-string exp :pretty nil)))
      (if (> (length s) *max-level-length*)
        (concatenate 'string (subseq s 0 (- *max-level-length* 3)) "...")
        s))
    ""))

;; -> dot

(defun node->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edge->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
          edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (node->dot nodes)
  (edge->dot edges)
  (princ "}"))

(defun uedge->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (node->dot nodes)
  (uedge->dot edges)
  (princ "}"))


;; -> png

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                    fname
                    :direction :output
                    :if-exists :supersede)
    (funcall thunk)))


(defun graph->png (fname nodes edges)
  (dot->png fname (lambda () 
                    (graph->dot nodes edges))))

(defun ugraph->png (fname nodes edges)
  (dot->png fname 
            (lambda () 
              (ugraph->dot nodes edges))))

;; test

(defparameter *test-nodes* '((a (This is a. the first char of alphabet.))
                             (b (This is b. the second order of alphabet.))
                             (c (This is c. I don't know it.))))

(defparameter *test-edges* `((a (b west door)
                                (c  upstairs ladder))
                             (b (a east door))
                             (c (a downstairs ladder))))

