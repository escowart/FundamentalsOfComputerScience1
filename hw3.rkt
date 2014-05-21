;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ps3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;;Edwin Cowart and Jonas Rosenzweig
;;;Problem Set 3

;;;Problem A1

;1.

(define-struct lecture-hall (number capacity))
;make-lecture-hall
;lecture-hall-number   
;lecture-hall-capacity

(define-struct automobile (year make model))
;make-automobile              
;automobile-year        
;automobile-make            
;automobile-model

(define-struct football-player (name position number))
;make-football-player
;football-player-name
;football-player-position
;football-player-number

(define-struct highway (lanes speed))
;make-highway 
;highway-lanes
;highway-speed

(define-struct shirt (material size color))
;make-shirt
;shirt-material
;shirt-size
;shirt-color

#|
2.
a lecture-hall is a structure: 
(make-lecture-hall (number capacity))
where number and capacity are numbers

an automobile is a structure: 
(make-automobile (year make model))
where year is a number and make and model are strings

a football-player is a structure: 
(make-football-player (name position number))
where number is a number and position and name are strings

a highway is a structure: 
(make-highway (lanes speed))
where lanes is a string and speed is a number

a shirt is a structure: 
(make-shirt ((material size color))
where material and color are strings and size is a number
3.

(struct-var (make-struct var1 var2))

|#


;;;Problem A2
(require 2htdp/image)
(require 2htdp/universe)

;;; a time-struct has hours and minutes
(define-struct time (hours minutes))
;; Time = (make-time Number Number)


;; Constraints:
;; The first number is always between 0 and 11;
;; the second number is always between 0 and 59.

;;; a time-struct has hours and minutes
;;;tock: time-struct -> time-struct
;;;It adds one minute to the time-struct

;(define (tock t)
; (cond[(< (time-minutes t) 59) ...]
;       [(and (>= (time-minutes t) 59) (>= (time-hours t) 11)) ...]
;       [else   ...]))


(define (tock t)
  (cond
    [(< (time-minutes t) 59) (make-time (time-hours t) (add1 (time-minutes t)))]
    ;If time in minutes is less than 59 it adds 1 to minutes
    [(and (>= (time-minutes t) 59) (>= (time-hours t) 11)) (make-time 0 0)]
    ;If time is 11:59 it sets time to 00:00
    [else (make-time (add1(time-hours t))  0)]))
    ;If time is #:59 and not 11:59 it adds one hour and sets minutes to 0

(check-expect (tock (make-time 0 0)) (make-time 0 1))
(check-expect (tock (make-time 1 59)) (make-time 2 0))
(check-expect (tock (make-time 11 59)) (make-time 0 0))


;;; a time-struct has hours and minutes
;;;time->text: time-struct -> text
;;;It converts a time-struct into a text displaying the time
;;;(make-time 0 0) will display 12:00 
;;;(make-time 1 10) will display 1:10 
;;;(make-time 11 59) will display 11:59
#|
(define (time->text t)
         (cond[(= (time-hours t) 0) ...]
              [else ...])
          (cond[(< (time-minutes t) 10) ...]
               [else ...])
|#

(define (time->text t)
  (text (string-append
         (cond[(= (time-hours t) 0) "12"]
              ;if hours is 0 it sets the text to display 12
              [else (number->string (time-hours t))])
              ;for everything aside from 12 it display the hours
         ":";adds a :
          (cond[(< (time-minutes t) 10) "0"]
          ;This puts the 0 infront of minutes making it look like normal time
              [else ""])
               ;anything greater than or equal to 10 does not add the 0
          (number->string (time-minutes t))) 50 "timesnewroman"))
   ;displays minutes-sets the time as a text with size 50 and times new roman

;;works with big bang:)

;;;Problem A3

;1.
;a ballPos-struct has an x and y position. Tells position of ball.
(define-struct ballPos (posx posy))

;2.
;a velocity-struct is a vector that has a velx and vely. 
;Tells vector velocity of ball.
(define-struct velocity (velx vely))
  
;3.
; a ball-struct has pos which is a ballPos-struct
;vel which is a velocity-struct
(define-struct ball (pos vel))

;4.

;;;ball-next: ball-struct -> ball-struct
;;;It goes to the next instance of the ball. 
;;;it adds the velocity vector to the position vector


(define (ball-next b)
  (make-ball 
   (make-ballPos (+ (ballPos-posx (ball-pos b)) (velocity-velx (ball-vel b)))
                           ;adds x velocity to the x coord
                 (+ (ballPos-posy (ball-pos b)) (velocity-vely (ball-vel b))))
                           ;adds y velocity to the y coord
                 (ball-vel b)));keeps velocity the same

(check-expect (ball-next (make-ball (make-ballPos 0 0) (make-velocity 1 2) ))
              (make-ball (make-ballPos 1 2) (make-velocity 1 2)))

;5.
;ball image places the ball at the coordinates on a 300 by 300 scene

;;;ball-image: ball-struct -> scene
;;;It places the ball onto an empty scene.

(define (ball-image b)
  (place-image 
   (circle 10 "solid" "red") 
   (ballPos-posx (ball-pos b)) (ballPos-posy (ball-pos b))
               ;calling the x and y coords of the ball
               (empty-scene 300 300)))
;:) Works

;6.
;;;ball-image: ball-struct and key -> ball-struct
;;;It converts an arrow press into velocity vector and puts it into the ball.
#|
(define (ball-change b key-hit)
             (cond
               [(key=? key-hit "left") ...]
               [(key=? key-hit "right")...]
               [(key=? key-hit "up")   ...]
               [(key=? key-hit "down") ...]
               [else ...]))
|#

(define (ball-change b key-hit)
  (make-ball  (ball-pos b);sets the x y coord to the same
             (cond
               [(key=? key-hit "left")  (make-velocity -10 0)]
               [(key=? key-hit "right") (make-velocity 10 0)]
               [(key=? key-hit "up")    (make-velocity 0 -10)]
               [(key=? key-hit "down")  (make-velocity 0 10)]
               [else (ball-vel b)])));sets it to the same if no key is pressed

;intial world for big bang
;(define some-initial-ball 
;   (make-ball (make-ballPos 150 150) 
;          (make-velocity 0 0)))
