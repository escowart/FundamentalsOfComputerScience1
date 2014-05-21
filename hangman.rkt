;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hangman) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; Hang-man
;;; Lose if lost 7

(require 2htdp/image)
(require 2htdp/universe)

;;; World is a (make-world String String Number)
(define-struct world (word guessed))

(define world0
  (make-world "" ""))

;;; wrong-list : World -> [Listof String]
;;; Gives the list of wrong things

(define (wrong-list w)
  (filter
   (λ (str) (not (member? str (explode (world-word w)))))
   (explode (world-guessed w))))

;;; right-list : World -> [Listof String]
;;; Gives the list of right things

(define (right-list w)
  (filter
   (λ (str) (member? str (explode (world-word w))))
   (explode (world-guessed w))))

;;; render-correct : World Image -> Image
;;; renders the world stuff

(define (render-correct w img)
  (local [(define (underliner n constant img)
            (cond [(zero? n) img]
                  [else
                   (place-image 
                    (line 25 0 'black)
                    (* n (/ 400 (add1 constant)))
                    500
                    (underliner (sub1 n) constant img))]))
          (define (render-underline n img)
            (cond 
              [(positive? n)
               (underliner n n img)]
              [else img]))
          (define (cr-alet word guess)
            (map 
             (λ (str) (list str (member? str guess)))
             word))
          (define (render-alet a-let-list l img)
            (foldr
             (λ (a-let im) 
               (cond [(first (rest a-let))
                      (place-image 
                       (text (first a-let) 30 'black)
                       (*  (/ 400 (add1 l)))
                       485
                       im))
             img
             a-let-list))
          (define (render-letters w img)
            (render-alet 
             (cr-alet (explode (world-word w)) (explode (world-guessed w)))
             (string-length (world-word w))
             img))
          ]
    (render-letters w
     (render-underline 
      (string-length (world-word w)) img))))

;;; render-guessed : String Image -> Image
;;; renders the guessed letter

(define (render-guessed str img w)
  (local [(define (list-num-op los n)
            (cond [(empty? los) empty]
                  [else 
                   (cons (list (first los) n)
                         (list-num-op (rest los) (+ 50 n)))]))]
    (foldr
     (λ (lsn img)
       (place-image
        (text (first lsn) 30 'black) (first (rest lsn)) 380 img))
     img
     (list-num-op
      (wrong-list w) 50))))

;;; render-world : World -> Image
;;; Renders the Goddam World

(define (render-world w)
  (local [(define (place-cond num img)
            (cond [(zero? num) img]
                  [(= 7 num) 
                   (place-image 
                    (line 10 10 'black) 190 105 
                    (place-image 
                     (line -10 10 'black) 190 105
                     (place-image 
                      (line 10 10 'black) 210 105
                      (place-image 
                       (line -10 10 'black) 210 105 (place-cond (sub1 num) img)))))]
                  [else (place-cond (sub1 num)
                                    (cond 
                                      [(= 1 num) 
                                       (place-image 
                                        (circle 30 'solid 'red) 200 105 img)]
                                      [(= 2 num) 
                                       (place-image 
                                        (line 0 150 'red) 200 150 img)]
                                      [(= 3 num) 
                                       (place-image 
                                        (line 50 50 'red) 225 175 img)]
                                      [(= 4 num) 
                                       (place-image 
                                        (line -50 50 'red) 175 175 img)]
                                      [(= 5 num) 
                                       (place-image 
                                        (line 50 50 'red) 225 250 img)]
                                      [(= 6 num) 
                                       (place-image 
                                        (line -50 50 'red) 175 250 img)]
                                      [else img]))]))]
    (render-correct w
     (render-guessed
      (wrong-list w)
      (place-cond
       (string-length (foldr (λ (a b) (string-append a b)) "" (wrong-list w)))
       (place-image
        (line 0 300 'black) 50  200
        (place-image
         (line 150 0 'black) 125 50
         (place-image
          (line 0 25 'black) 200  62.5
          (empty-scene 400 600))))) w))))

(render-world (make-world "stupid" "aeioustr"))