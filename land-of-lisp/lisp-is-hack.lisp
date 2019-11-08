;; ############################################################
;;  They are not-lisp-like commands in common lisp,
;; but are strong enough to be included.
;; ############################################################

;; ########################################
;; LOOP
;; ########################################
(defun my-loops ()
  (progn
    (print (loop for i below 5 sum i))
    (print (loop for i from 5 to 10 sum i))
    (print (loop for i in '(100 20 3) sum i))
    (loop for i below 5 do (print i))
    (print (loop for i below 10 when (oddp i) sum i))
    (print (loop for i from 0 do (print i) when (= i 5) return 'BOMB))
    (print (loop for i in '(1 2 3 4 5 6) collect (* i i)))
    (print (loop for x below 10 for y below 10 collect(+ x y)))
    (print (loop for i from 0 for day in '(MON TUE WED THU FRI SAT SUN) collect(cons i day)))
    ))

;; ########################################
;; FORMAT
;; ########################################
(defun my-format-str ()
  (format 
    t 
    "only ~$ dollars more! > console. | "
    1.5) 
  (format 
    nil 
    "only ~$ dollars more! > string. | " 
    1.5)
  (format
    t
    "I'm printing ~s in this sentence. | "
    "foo")
  (format
    t
    "I'm printing ~a in this. | "
    "foo")
  (format
    t
    "width of 10 (left) :~10a:"
    "foo"
    )
  (format
    t
    "width of 10 (right) :~10@a:"
    "foo"
    )
  (format
    t
    "padding with ! :~,,4,'!a:"
    "foo"
    )
  )

(defun my-format-dec ()
  (format
    t
    "~d is "
    1000
    )
  (format
    t
    "~x in hex, "
    1000
    )
  (format
    t
    "~b in bin. ~&"
    1000
    )
  (format
    t
    "1000000 is ~:d in grouped, "
    1000000
    )
  (format
    t
    "~10d in 10width, "
    1000000
    )
  (format
    t
    "~10,'xd in padded."
    1000000
    )
  )

(defun my-format-float ()
  (format
    t
    "PI is ~4f in 4width ~&"
    pi
    )
  (format
    t
    "PI is ~,4f in 4width after the decimal point ~&"
    pi
    )
  (format
    t
    "PI is ~8,,4f when multiplied with E4 ~&"
    pi
    )
  (format
    t
    "PI is ~$ as price. ~&"
    pi
    )
  )

(defun my-format-table ()
  (labels ((random-animals ()
             (nth (random 5)
                  '("dog" "tick" "tiger" "walrus" "kangaroo"))))
    (loop repeat 10
          do (format t "~5t~a ~15t~a ~25t~a~%"
               (random-animals)
               (random-animals)
               (random-animals)))
    (loop repeat 10
          do (format t "~30<~a~;~a~;~a~>~%"
               (random-animals)
               (random-animals)
               (random-animals)))
    
    (loop repeat 10
          do (format t "~30:@<~a~>~%"
               (random-animals)))
    (loop repeat 10
          do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
               (random-animals)
               (random-animals)
               (random-animals)))
    ))

(defun my-format-loop ()
  (labels ((random-animals ()
             (nth (random 5) 
                  '("dog" "tick" "tiger" "walrus" "kangaroo"))))
    (let ((animals (loop repeat 10 collect (random-animals))))
      animals
      (format t "~{I see a ~a and ~a! ~}" animals)
      )
    )
  )

(defun my-format-crazy ()
  (format t "|~{~<|~%|~,33:;~2d ~>~}|"
          (loop for x below 100 collect x)
          )
  )

;; ########################################
;; STREAM
;; ########################################

; stdio
(defun my-iostream ()
  (output-stream-p *standard-output*)
  (write-char #\x *standard-output*)
  (input-stream-p *standard-input*)
  (read-char *standard-input*))

; file stream
(defun my-fstream ()
  (with-open-file (my-stream "test.data.txt"
                             :direction :output)
    (print "my data" my-stream))
  (let ((animal-noises '((dog . woof)
                         (cat . meow))))
    (with-open-file (my-stream "test.map.txt"
                               :direction :output) 
      (print animal-noises my-stream))
    (with-open-file (my-stream "test.map.txt"
                               :direction :input)
      (read my-stream)))
  (with-open-file (my-stream "test.data.txt" 
                             :direction :output
                             :if-exists :error ))
  (with-open-file (my-stream "test.data.txt"
                             :direction :output
                             :if-exists :supersede)
    (print "my override data" my-stream)))

; socket
(defun my-socket ()
  ;; CONNECT
  ; ON THE SERVER
  (defparameter my-socket (socket-server 4321))
  (defparameter my-stream (socket-accept my-socket))
  ; ON THE CLIENT
  (defparameter my-stream (socket-connect 4321 "127.0.0.1"))
  ;; TALK
  (read my-stream)
  (write "message here." my-stream)
  ;; CLOSE
  ; ON THE SERVER and CLIENT
  (close my-stream)
  ; ON THE SERVER
  (socket-server-close my-socket))

; string
(defun my-sstream ()
  (let ((foo (make-string-output-stream)))
    (princ "this is go into foo." foo)
    (princ "this 2." foo)
    (get-output-stream-string foo)
    )
  (with-output-to-string (*standard-output*)
    (princ "The sum of")
    (princ 5)
    (princ " and ")
    (princ 2)
    (princ " is ")
    (princ (+ 2 5))
    )
  )
(my-sstream)






