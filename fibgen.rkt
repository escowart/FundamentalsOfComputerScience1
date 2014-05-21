;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fibgen) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; Fibanachy Curve
(require 2htdp/image)
(require 2htdp/universe)
;;; A World is a [Listof Number]

(define world0 '())

;;; next-term : World -> World
;;; adds the next term in the sequence to the end of the current sequence

(define (next-term lon)
  (cond [(or (empty? lon) (empty? (rest lon))) (append lon '(1))]
        [(empty? (rest (rest lon))) (append lon (list (+ (first lon) (first (rest lon)))))]
        [else (cons (first lon) (next-term (rest lon)))]))

(define cc .7)
;;; render-terms : World -> Image
;;; renders the sequence

(define (render-terms lon)
  (local [(define (dox n)
            (cond [(or (= n 0) (= n 3)) +]
                  [else -]))
          (define (doy n)
            (cond [(or (= n 2) (= n 3)) +]
                  [else -]))
          (define (tc n)
            (cond [(= n 0) cc]
                  [(= n 2) (- cc)]
                  [else 0]))
          (define (bc n)
            (cond [(= n 3) cc]
                  [(= n 1) (- cc)]
                  [else 0]))
          (define (rt-h lon n x y img)
            (cond [(empty? lon) img]
                  [else (add-curve
                         (rt-h (rest lon) (modulo (add1 n) 4) ((dox n) x (first lon)) ((doy n) y (first lon)) img)
                         x y 0 (tc n)
                         ((dox n) x (first lon)) ((doy n) y (first lon)) 0 (bc n)
                         'black
                         )]))]
    (rt-h lon 0 500 500 (empty-scene 1000 1000))))


;;; Run the Program
(big-bang world0
          (on-tick next-term .3)
          (to-draw render-terms))