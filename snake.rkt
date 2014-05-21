;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;;Snake - Game

(require 2htdp/image)
(require 2htdp/universe)

;;;Coordinates are equiviant to 10(x - 1/2) 100 - 10(y - 1/2)
;;;Scene is 1000 x 1000

(define SCENE-SIZE 1000) 

(define (create-blank n width height)
  (cond[(<= n 0) (empty-scene width height)]
       [else (place-image
              (line width 0 "black") 
              (/ width 2) 
              (* 50 n)
              
              (place-image
              (line  0 height "black") 
              (* 50 n)
              (/ height 2) 
              (create-blank (- n 1) width height)))]))



;;;A BLANK-SCENE

(define BLANK-SCENE
  (create-blank 20 SCENE-SIZE SCENE-SIZE))


;;;PART is (make-posn Number Number)
(define (create-part x y)
  (make-posn x y))

;;;FOOD is (make-posn Number Number)
(define (create-food x y)
  (make-posn x y))

;;;A LoPS is either:
;;; - empty
;;; - (cons PART

;;;DIR is either a 'up 'down 'left 'right
;;;A SNAKE is a (make-snake LoPS DIR)
;;; The first part of the LoPS is the head
(define-struct snake (part dir))

;;;image of a FOOD
(define FOOD-IMG (circle 20 'solid 'orange))
  
;;;image of a PART
(define PART-IMG (circle 20 'solid 'blue))

;;;WORLD is a (make-world SNAKE FOOD)
(define-struct world (snake food))

(define start-part (create-part 10 10))
(define start-snake (make-snake (cons start-part empty) 'up))
(define start-food (create-part (+ 1 (random 20)) (+ 1 (random 20))))
(define start-world (make-world start-snake start-food))

;;; place-posn-img : Posn Image Image -> Image

;;; next-world : WORLD -> WORLD
;;; eat? World -> Boolean
;;; Grow : World -> World
;;; Slither : World -> World
;;; Game-Over : World -> World
;;; Self-Coll? : World -> Boolean
;;; Outside? : World -> Boolean

;;; render-world : WORLD -> Image
;;; render-snake : SNAKE Image -> Image
;;; render-LoPS : LoPS Image -> Iamge
;;; render-food : FOOD Image -> Iamge

;;; change-dir : World Key -> World

;;; place-posn-img : Posn Image Image -> Image
;;; places an image on another image on coord

(define (place-posn-img img pos scene)
  (place-image img
               (* 50 (- (posn-x pos) .5))
               (- SCENE-SIZE (* 50 (- (posn-y pos) .5)))
               scene))



;;; posn=? : Posn Posn -> Boolean
;;; Test whether two Posns are equal

(define (posn=? a b)
  (and (= (posn-x a) (posn-x b))
       (= (posn-y a) (posn-y b))))

(check-expect (posn=? (make-posn 1 1) (make-posn 1 1)) true)
(check-expect (posn=? (make-posn 1 1) (make-posn 0 1)) false)

;;adders

(define (add1up pos)
  (make-posn (posn-x pos) (add1(posn-y pos))))

(define (add1down pos)
  (make-posn (posn-x pos) (- (posn-y pos) 1)))

(define (add1right pos)
  (make-posn (add1(posn-x pos)) (posn-y pos)))

(define (add1left pos)
  (make-posn (- (posn-x pos) 1) (posn-y pos)))


;;; next-part : LoPS -> Posn
;; tells the next posn

(define (next-part s)
  (cond[(empty? (snake-part s)) empty]
       [(symbol=? (snake-dir s) 'up)    (add1up    (first (snake-part s)))]
       [(symbol=? (snake-dir s) 'down)  (add1down  (first (snake-part s)))]
       [(symbol=? (snake-dir s) 'right) (add1right (first (snake-part s)))]
       [else                            (add1left  (first (snake-part s)))]));'left
  

;;; eat? World -> Boolean
;;; test whether the snake eats the food

(define (eat? w)
  (posn=? (next-part (world-snake w)) (world-food w)))

(check-expect (eat?
               (make-world (make-snake (cons (make-posn 5 4) empty) 'up) 
                           (make-posn 5 5))) true)

(check-expect (eat?
               (make-world (make-snake (cons (make-posn 5 4) empty) 'down) 
                           (make-posn 15 5))) false)

(check-expect (eat?
               (make-world (make-snake (cons (make-posn 5 4) empty) 'right) 
                           (make-posn 15 5))) false)

(check-expect (eat?
               (make-world (make-snake (cons (make-posn 5 4) empty) 'left) 
                           (make-posn 15 5))) false)

;;; test-lops? Posn LoPS -> Boolean
;;; checks whether the LoPS contains the Posn

(define (test-lops? posn lops)
  (cond[(empty? lops) false]
       [(posn=? posn (first lops)) (or true (test-lops? posn  (rest lops)))]
       [else (or false (test-lops? posn (rest lops)))]))

(check-expect (test-lops? (make-posn 1 1) (cons start-part empty)) false)

;;; create-new-part: LoPS -> Posn
;;; creates a new food not in the snake

(define (create-new-part lops rand)
  (cond[(not (test-lops? rand lops)) rand]
       [else (create-new-part 
              lops 
              (make-posn (add1 (random 20)) (add1 (random 20))))]))

(create-new-part (cons start-part empty) (make-posn 1 1))


;;; Grow : World -> World
;;; Grows the Snake if it eats and places a new food

(define (Grow w)
  (make-world (make-snake 
               (cons (next-part (world-snake w))
                     (snake-part (world-snake w)))
               (snake-dir (world-snake w)))
              (create-new-part (cons (next-part (world-snake w))
                     (snake-part (world-snake w)))
             (make-posn (add1 (random 20)) (add1 (random 20))))))
 
;(check-expect (Grow 
;               (make-world (make-snake  (cons (make-posn 5 5) empty) 'up) (make-posn 4 6)))
;              (make-world (make-snake (cons (make-posn 5 6) (cons (make-posn 5 5) empty)) 'up) (make-posn 4 6)))

;;; slither-lops : Losn
;;; subtracts the last one before the empty

(define (slither-lops lops)
  (cond[(empty? lops) empty]
       [(empty? (rest lops)) empty]
       [else (cons (first lops) (slither-lops (rest lops)))]))

(check-expect (slither-lops (cons (make-posn 5 6) (cons (make-posn 5 5) empty)))
              (cons (make-posn 5 6) empty))

(check-expect (slither-lops empty)
              empty)


;;;Slither : World -> World
;;slithers the snake 

(define (Slither w)
  (make-world (make-snake
               (cons (next-part (world-snake w))
               (slither-lops (snake-part (world-snake w))))
               (snake-dir (world-snake w))) 
              (world-food w)))

(check-expect (Slither
               (make-world (make-snake  (cons (make-posn 5 6) (cons (make-posn 2 2) empty)) 'up) (make-posn 4 6)))
              (make-world (make-snake (cons (make-posn 5 7) (cons (make-posn 5 6) empty)) 'up) (make-posn 4 6)))

;;;Self-LoPS-Test? : PART LoPS -> Boolean
;;;Tests Whether the first piece will collide with anything

(define (Self-LoPS-Test? sp lops)
  (cond[(empty? lops) false]
       [else (or (posn=? sp (first lops)) (Self-LoPS-Test? sp (rest lops)))]))

(check-expect (Self-LoPS-Test? 
               (make-posn 2 2)
               (cons (make-posn 5 6) (cons (make-posn 2 2) empty)))
              true)
               
(check-expect (Self-LoPS-Test? 
               (make-posn 5 2)
               (cons (make-posn 5 6) (cons (make-posn 2 2) empty)))
              false)


;;; Self-Coll? : World -> Boolean
;;; Test whether it collides with himself

(define (Self-Coll? w)
  (Self-LoPS-Test? (next-part (world-snake w)) (snake-part (world-snake w))))
  
(check-expect (Self-Coll?
                (make-world (make-snake  (cons (make-posn 5 6) (cons (make-posn 5 7) empty)) 'up) (make-posn 4 6)))
              true)

(check-expect (Self-Coll?
                (make-world (make-snake  (cons (make-posn 5 6) (cons (make-posn 2 2) empty)) 'up) (make-posn 4 6)))
              false)

;;; Outside? : World -> Boolean
;;; Test whether it goes off screne

(define (Outside? w)
  (not (and (<= 1 (posn-x (next-part (world-snake w))) 20)
           (<= 1 (posn-y (next-part (world-snake w))) 20))))

(check-expect (Outside?
                (make-world (make-snake  (cons (make-posn 0 0) (cons (make-posn 5 7) empty)) 'up) (make-posn 4 6)))
              true)

(check-expect (Outside?
                (make-world (make-snake  (cons (make-posn 1 0) (cons (make-posn 2 2) empty)) 'up) (make-posn 4 6)))
              false)

;;; Score : lops -> Number
;;; Scoring system that prints 

(define (Score lops)
  (cond[(empty? lops) 0]
       [else (add1 (Score (rest lops)))]))

(check-expect (Score (cons (make-posn 5 4) empty)) 1)

;;; Game-Over : World -> World
;; Exicutes game over code

(define (Game-Over w)
  (cond[(not (= (posn-y (first (snake-part (world-snake w)))) -1))
        (make-world (make-snake (cons (make-posn (Score (snake-part (world-snake w))) -1)
                                (snake-part (world-snake w))) 'gameover)
              (world-food w))]
       [else w]))

;(check-expect (Game-Over 
;              (make-world (make-snake (cons (make-posn 5 4) empty) 'up) (make-posn 2 8)))
;               (make-world (make-snake (cons (make-posn -1 -1) empty) 'gameover) (make-posn 1 -1)))

;;; next-world : WORLD -> WORLD
;;; This makes the snake move

(define (next-world w)
  (cond[(eat? w) (Grow w)]
       [(or (Self-Coll? w) (Outside? w)) (Game-Over w)]
       [else (Slither w)]))

(check-expect (next-world 
               (make-world (make-snake (cons (make-posn 5 4) empty) 'up) (make-posn 2 8)))
              (make-world (make-snake (cons (make-posn 5 5) empty) 'up) (make-posn 2 8)))

;(check-expect (next-world 
;               (make-world (make-snake (cons (make-posn 0 0) empty) 'up) (make-posn 2 8)))
;              (make-world (make-snake empty 1) (make-posn -1 -1)))

;(check-expect (next-world 
;               (make-world (make-snake (cons (make-posn 5 4) empty) 'up) (make-posn 5 5)))
;              (make-world (make-snake (cons (make-posn 5 5) (cons (make-posn 5 4) empty)) 'up) (make-posn 5 5)))


;;; render-LoPS : LoPS Image -> Iamge
;;; Puts a Lops on a img

(define (render-LoPS lops img)
  (cond[(empty? lops) img]
       [else (place-posn-img PART-IMG (first lops) (render-LoPS (rest lops) img))]))


;;; render-food : FOOD Image -> Image
;;; places food image on another

(define (render-food pos img)
  (place-posn-img FOOD-IMG pos img))

;;; render-snake : SNAKE Image -> Image
;;; Renders the snake image

(define (render-snake snake img)
  (render-LoPS (snake-part snake) img))

;;; render-world : WORLD -> Image
;;; Renders the world

(define (render-world world)
  (cond[(symbol=? 'gameover (snake-dir (world-snake world)))
                  (place-image (text "Game Over" 50 "black") (/ SCENE-SIZE 2) (/ SCENE-SIZE 2)
                               (place-image (text (string-append "Score   " 
                                                                 (number->string (posn-x 
                                                                  (first (snake-part (world-snake world))))))
                                                  50 
                                                  "black") 
                                            (/ SCENE-SIZE 2) (/ SCENE-SIZE 1.75)
                                            (render-snake (world-snake world)
                                            (render-food (world-food world) BLANK-SCENE))))]
       [else
        (render-snake (world-snake world)
                (render-food (world-food world)
                             BLANK-SCENE))]))

;;; change-dir : World Key -> World
;;; Changes dirrection on key press

(define (change-dir w key)
  (make-world
   (make-snake
    (snake-part (world-snake w))
    (cond[(key=? key "up")    'up   ]
       [(key=? key "down")  'down ]
       [(key=? key "left")  'left ]
       [(key=? key "right") 'right]
       [else (snake-dir (world-snake w))]))
   (world-food w)))

(check-expect (change-dir 
               (make-world (make-snake (cons (make-posn 0 0) empty) 'up) (make-posn 2 8)) "right")
              (make-world (make-snake (cons (make-posn 0 0) empty) 'right) (make-posn 2 8)))
       

;;;big bang

(big-bang start-world
          (on-tick next-world .1)
          (to-draw render-world)
          (on-key change-dir))


