;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; Edwin Cowart & Jo Crotty

;;; --- A1 ---

;; sort-a : [Listof Number]  ->  [Listof Number] 
;; to construct a list with all items from alon in increasing order
#;
(define (sort-a alon)
  (local ((define (insert an alon)
            (cond [(empty? alon) (list an)]
                  [( < an (first alon)) (cons an alon)]
                  [else (cons (first alon) 
                              (insert an (rest alon)))])))
    (cond [(empty? alon) empty]
          [else (insert (first alon) (sort-a (rest alon)))])))

;;; sort-a : [Listof Number] [Number -> Boolean]  ->  [Listof Number]
;; to construct a list with all items from alon in an order

(define (sort-a lon op)
  (local ((define (insert n lon)
            (cond [(empty? lon) (list n)]
                  [(op n (first lon)) (cons n lon)]
                  [else (cons (first lon) 
                              (insert n (rest lon)))])))
    (foldr insert empty lon)))

(define (sort-ascending lon)
  (sort-a lon <))

(check-expect (sort-ascending '( 3 9 4 2 1))
              (list 1 2 3 4 9))

(define (sort-descending lon)
  (sort-a lon >))

(check-expect (sort-descending '( 3 9 4 2 1)) 
              (list 9 4 3 2 1))

;;; --- A2 ---

;;; (lambda (x y) (x y y))
;;;                   illegal: lambda can only have one output
;;; (lambda () 10)    illegal: lambda needs an arguement
;;; (lambda (x) x)      legal: prints out x
;;; (lambda (x y) x)    legal: prints out x
;;; (lambda x 10)     illegal: lambda requires the arguements in parrrens

;;; --- A3 ---

;;; a Nat-Num is either
;;; - 0
;;; - (add1 Nat-Num)

#;
(define (nat-temp n)
  (cond [(zero? n) ...]
        [else ...(nat-temp (sub1))...]))

;;; a-fives : Nat-Num -> Number
;;; finds the correspending number in a series

(define (a-fives n)
  (cond [(zero? n) 8]
        [else (+ 5 (a-fives (sub1 n)))]))

(check-expect (a-fives 0) 8)
(check-expect (a-fives 1) 13)

;;; seq-a-fives : Nat-Num -> [Listof Number]
;;; create n number of terms in the series

(define (seq-a-fives n)
  (build-list n a-fives))

(check-expect (seq-a-fives 4) (list 8 13 18 23))

;;; --- A4 ---

;; A Grade is: (make-grade Symbol Number)
(define-struct grade (letter num))

;; The Symbol in a Grade represents

;;    'A  >= 90
;;    'B  >= 80
;;    'C  >= 70
;;    'D  >= 60 
;;    'F  < 60

;; A [Listof Grade] ...
(define grades 
  (list (make-grade 'D 62) (make-grade 'C 79) 
        (make-grade 'A 93) (make-grade 'B 84) 
        (make-grade 'F 57) (make-grade 'F 38) 
        (make-grade 'A 90) (make-grade 'A 95)
        (make-grade 'C 76) (make-grade 'A 90) 
        (make-grade 'F 55) (make-grade 'C 74)
        (make-grade 'A 92) (make-grade 'B 86) 
        (make-grade 'F 43) (make-grade 'C 73)))

;;; log->lon : [Listof Grade] -> [Listof Number]
;;; extracts the grades from a list of grades

(define (log->lon log)
  (map grade-num log))

(check-expect (log->lon grades)
              (list 62 79 93 84 57 38 90 95 76 90 55 74 92 86 43 73))

;;; best-grade : [Listof Grade] -> Grade
;;; What is the greatest grade?

(define (best-grade log)
  (foldr 
   (λ (a b) (cond [(< (grade-num a) 
                      (grade-num b))
                        b]
                  [else a]))
   (make-grade 'F 0) log))

(check-expect (best-grade grades) (make-grade 'A 95))

;;; just-As : [Listof Grade] -> [Listof Grade]
;;; filters everything that is not an A

(define (just-As log)
  (filter 
          (lambda (g) (symbol=? 'A (grade-letter g)))
          log))

(check-expect 
 (just-As grades)
 (list
  (make-grade 'A 93)
  (make-grade 'A 90)
  (make-grade 'A 95)
  (make-grade 'A 90)
  (make-grade 'A 92)))

;;; all-pass? : [Listof Grade] ->  Boolean
;;; Is every grade a passing one?

(define (all-pass? log)
  (andmap 
   (lambda (g) (not (symbol=? 'F (grade-letter g))))
          log))

(check-expect (all-pass? grades) false)
(check-expect (all-pass? 
               (list (make-grade 'D 62) (make-grade 'C 79) 
                     (make-grade 'A 93) (make-grade 'B 84) 
                     (make-grade 'A 90) (make-grade 'A 95)
                     (make-grade 'C 76) (make-grade 'A 90)  
                     (make-grade 'C 74) (make-grade 'A 92) 
                     (make-grade 'B 86)(make-grade 'C 73))) true)

;;; bonus : [Listof Grade] -> [Listof Grade]
;;; Adds a bonus of five to all grades

(define (bonus log)
  (map
   (lambda (g)
     (make-grade (grade-letter g)
                 (+ 5 (grade-num g))))
   log))

(check-expect (bonus grades)
              (list
               (make-grade 'D 67)
               (make-grade 'C 84)
               (make-grade 'A 98)
               (make-grade 'B 89)
               (make-grade 'F 62)
               (make-grade 'F 43)
               (make-grade 'A 95)
               (make-grade 'A 100)
               (make-grade 'C 81)
               (make-grade 'A 95)
               (make-grade 'F 60)
               (make-grade 'C 79)
               (make-grade 'A 97)
               (make-grade 'B 91)
               (make-grade 'F 48)
               (make-grade 'C 78)))
                          