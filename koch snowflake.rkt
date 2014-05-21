;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname flake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)
;;; Angle in radians 0 to 2pi

(define SIZE (expt 3 5.5))
(define list0 (list pi (* pi 5/3) (* pi 1/3) ))

;;; replace : [Listof Angle] -> [Listof Angle]
;;; Replaces all angles with four

(define (replace loa)
  (local [(define (make-angles+ loa)
            (map
             (λ (a) (cond [(< a 0) (+ (* 2 pi) a)]
                          [else a])) loa))
          (define (replace-a a)
            (list a (- a (* 1/3 pi)) (+ a (* 1/3 pi)) a))]
    (foldr
     append
     empty
     (map replace-a loa))))

;;; flatten : [Listof Angle] -> [Listof Angle]
;;; Replaces all angles with four

(define (flatten loa)
  (cond [(< (length loa) 4) loa]
        [(= (length loa) 4) (list (first loa))]
        [else (append
               (flatten (list 
                         (first loa) 
                         (first (rest loa))
                         (first (rest (rest loa)))
                         (first (rest (rest (rest loa))))))
               (flatten (rest (rest (rest (rest loa))))))]))
                        

;;; render-loa : Number Number [Listof Angle] 'Symbol Image -> Image
;;; Renders a list of angles

(define (render-loa x y loa c img)
  (local[(define side (expt 3 (/ (log (/ (length loa) 3)) (log 4))))
         (define (render-help x y lo c img size)
           (cond [(empty? lo) img]
                 [else
                  (add-line
                   (render-help 
                    (+ x (* size (cos (first lo))))
                    (+ y (* size (sin (first lo))))
                    (rest lo)
                    c
                    img size)
                   x
                   y
                   (+ x (* size (cos (first lo))))
                   (+ y (* size (sin (first lo))))
                   c)]))]
    (render-help (+ x (/ SIZE 2))
                 (+ y (* (/ SIZE 2) (expt 3 -1/2)))
                 loa c img (/ SIZE side))))

;;; render-world : [Listof Angle] -> Image
;;; renders the world

(define (render-world loa)
  (render-loa SIZE SIZE loa 'blue 
            (empty-scene (* 2 SIZE) (* 2 SIZE))))

;;; key-h : [Listof Angle] -> [Listof Angle]
;;; operates the keys

(define (key-h loa key-e)
  (cond [(key=? key-e "up")   (replace loa)]
        [(key=? key-e "down") (flatten loa)]
        [else loa]))

(big-bang list0
            (to-draw render-world)
            (on-key key-h))