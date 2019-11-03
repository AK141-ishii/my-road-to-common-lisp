(setf *print-circle* t)

(defun my-conscell-1 ()
  (cons 1 (cons 2 (cons 3 (cons 4 nil)))))

(defun my-dotlist-1 ()
  (cons 1 (cons 2 (cons 3 4))))

(defun my-pair-1 ()
  (cons 1 2))

(defparameter my-circle (list 1 2 3))
(setf (cdddr my-circle) my-circle)

(defparameter my-alist '((b . double)
                         (l . small)
                         (j . med)))
(push '(l . large) my-alist)

(defparameter my-tree '((a (ant)
                           (axio)
                           (amp))
                        (b (bat)
                           (bit)
                           (bot))
                        (c (con)
                           (cons))))

















