; ######################################################################### ; - ORK BATTLE GAME - ;  You are a knight battling against 12 enemies like ork, hiddra...
;  Let's survive this situation using OBJECT System of defmethod/defstruct.
; #########################################################################

; ########################################
; GLOBAL VARIABLES
; ########################################

; player's status
(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

; monster's general info
(defparameter *monsters* nil)         ; array
(defparameter *monster-builders* nil) ; list
(defparameter *monster-num* 12)

; ########################################
; MAIN
; ########################################

(defun ork-battle ()
  (init-monster)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. GAME OVER."))
  (when (monsters-dead)
    (princ "CONGRATULATIONS! You have vaniquished all of your foes.")))

(defun game-loop()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (mon)
           (or (monster-dead mon) (monster-attack mon)))
         *monsters*)
    (game-loop)))

; ########################################
; Function : Utility
; ########################################

(defun randval (n)
  (1+ (random (max n 1))))

; ########################################
; Function : Player
; ########################################

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with  a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 3)))))
         (princ "Your double swing has a strength of")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))

(defun random-monster ()
  (let ((pick-mon (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead pick-mon)
      (random-monster)
      pick-mon)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
      (progn (princ "That is not a valid monster number.")
             (pick-monster))
      (let ((mon (aref *monsters* (1- x))))
        (if (monster-dead mon)
          (progn (princ "That monster is already dead.")
                 (pick-monster))
          mon)))))

; ########################################
; Function : Monsters
; ########################################

(defun init-monster()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

(defun monster-dead (mon)
  (<= (monster-health mon) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((cnt 0))
    (map 'list
         (lambda (mon)
           (fresh-line)
           (princ "  ")
           (princ (incf cnt))
           (princ ". ")
           (if (monster-dead mon)
             (princ "**dead**")
             (progn (princ "(Health=")
                    (princ (monster-health mon))
                    (princ ") ")
                    (monster-show mon))))
         *monsters*)))

; ########################################
; Structs : Monster
; ########################################

; Shared struct / methods
(defstruct monster (health (randval 10)))
(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
    (progn (princ "You killed the ")
           (princ (type-of m))
           (princ " !"))
    (progn (princ "You hit the ")
           (princ (type-of m))
           (princ ", knocking off ")
           (princ x)
           (princ " health points! "))))
(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))
(defmethod monster-attack (m))

; evil ork
(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)
(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))
(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knock off ")
    (princ x)
    (princ " of your health points.")
    (decf *player-health* x)))

; malicious hydra
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)
(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))
(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
    (princ "The corpse of the fully decapitated and decapacitated hydra falls to the fllor!")
    (progn (princ "You loop off ")
           (princ x)
           (princ " of the hydra's heads!"))))
(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head!")
    (incf (monster-health m))
    (decf *player-health* x)))

; slime-mold
(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)
(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))
(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wrap around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also sguirts in your face, taking away a health point!")
      (decf *player-health*))))

; brigand
(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)
(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with his whip, taking off 2 agility points! ")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
           (decf *player-strength* 2)))))


(ork-battle)












