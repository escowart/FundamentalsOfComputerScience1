;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; Edwin Cowart + Jo Crotty
;;; Homework 12
;;; Problem A1

;;; Part 1

;;; prime? : Number -> Boolean
;;; Is the number prime? (Input Interger > 1)

(define (prime? n)
  (local [(define (prime-helper? m c)
            (cond [(< m c) true]
                  [(zero? (remainder n c)) false]
                  [else (prime-helper? m (add1 c))]))]
               (prime-helper? (floor (sqrt n)) 2)))


(check-expect (prime? 27644437) true)
(check-expect (prime?        2) true)
(check-expect (prime?        3) true)
(check-expect (prime?        4) false)
(check-expect (prime? 27644435) false)
(check-expect (prime? 0) false)

;;; Part 2

;;; Nat-Num is either:
;;; - 0
;;; - (add1 Nat-Num)


;;; list-primes : Nat-Num -> [Listof Number]
;;; Create a list of prime numbers up to a given number

(define (list-primes.acc nat)
  (local [(define (list-primes-helper n lop)
            (local [(define prime-member? 
                      (not (ormap (λ (p) (zero? (remainder n p))) lop)))]
            (cond [(and (= n nat) prime-member?) (cons n lop)]
                  [(= n nat) lop]
                  [prime-member? (list-primes-helper (add1 n) (cons n lop))]
                  [else (list-primes-helper (add1 n) lop)])))]
    (cond [(< nat 2) empty]
          [else (reverse (list-primes-helper 2 '()))])))

;;; Sieve of Eratostenes
(define (list-primes nat)
  (local [(define (lp-helper lon)
            (cond [(or (empty? lon) (< (floor (sqrt nat)) (first lon))) lon]
                  [else 
                   (cons (first lon)
                         (lp-helper
                          (filter 
                          (λ (n) (not (zero? (remainder n (first lon)))))
                          lon)))]))]
    (lp-helper (range 2 (add1 nat) 1))))

(check-expect (list-primes.acc 0)  '())
(check-expect (list-primes.acc 1)  '())
(check-expect (list-primes.acc 2)  '(2))
(check-expect (list-primes.acc 3)  '(2 3))
(check-expect (list-primes.acc 4)  '(2 3))
(check-expect (list-primes.acc 5)  '(2 3 5))
(check-expect (list-primes.acc 30) '(2 3 5 7 11 13 17 19 23 29))

(check-expect (list-primes 0)  '())
(check-expect (list-primes 1)  '())
(check-expect (list-primes 2)  '(2))
(check-expect (list-primes 3)  '(2 3))
(check-expect (list-primes 4)  '(2 3))
(check-expect (list-primes 5)  '(2 3 5))
(check-expect (list-primes 30) '(2 3 5 7 11 13 17 19 23 29))

;;; Problem A2

;;; make-palindrome : String -> String
;;; Create a palindrome by mirroring the last letter
;;; Cannot take empty as input string

(define (make-palindrome s)
  (string-append s (implode (rest (reverse (explode s))))))

(define (make-palindrome.acc s)
  (local[(define (mp-helper str acc)
           (cond [(string=? "" str) acc]
                 [else (mp-helper (substring str 1)
                                  (string-append (substring str 0 1) acc))]))]
    (string-append s (mp-helper (substring s 0 (sub1 (string-length s))) ""))))


(check-expect (make-palindrome     "abcde") "abcdedcba")
(check-expect (make-palindrome         "a")         "a")
(check-expect (make-palindrome.acc "abcde") "abcdedcba")
(check-expect (make-palindrome.acc     "a")         "a")

;;; is-palindrome? : String? -> Boolean
;;; Is the word a palindrome?

(define (is-palindrome? s)
  (string=? s (implode (reverse (explode s)))))

(define (is-palindrome?.acc s)
  (local[(define (reverse-string str acc)
           (cond [(string=? "" str) acc]
                 [else 
                  (reverse-string (substring str 1)
                                  (string-append (substring str 0 1) acc))]))]
    (string=? s (reverse-string s ""))))

(check-expect (is-palindrome?     "Hello")    false)
(check-expect (is-palindrome?     "A")         true)
(check-expect (is-palindrome?     "HellolleH") true)
(check-expect (is-palindrome?.acc "Hello")    false)
(check-expect (is-palindrome?.acc "A")         true)
(check-expect (is-palindrome?.acc "HellolleH") true)

;;; Problem A3

;;; Part 1

;;; fibonacci : Number -> Number
;;; What is the number in the sequence at the given index?

(define (fibonacci n)
  (cond [(zero? n) 0]
        [(= n 1)   1]
        [else (+ (fibonacci (sub1 n)) (fibonacci (- n 2)))]))

(check-expect (fibonacci 0) 0)
(check-expect (fibonacci 1) 1)
(check-expect (fibonacci 11) 89)

;;; Part 2

;;; fibonacci.v2 : Number -> Number
;;; What is the number in the sequence at the given index?

(define (fibonacci.v2 n)
  (local [(define (v2-helper n a b)
            (cond [(zero? n) a]
                  [else (v2-helper (sub1 n) b (+ a b))]))]
    (v2-helper n 0 1)))

(check-expect (fibonacci.v2 0) 0)
(check-expect (fibonacci.v2 1) 1)
(check-expect (fibonacci.v2 11) 89)

;;; Part 3

;;; list-fibonacci : Nat-Num -> {Listof Number]
;;; Make a list of the fibonacci sequence

(define (list-fibonacci nat)
  (local [(define (lf-helper n c)
            (cond [(zero? nat) c]
                  [(= nat n) c]
                  [(zero? n) (lf-helper (add1 n) (cons 1 c))]
                  [else (lf-helper (add1 n) 
                                   (cons (+ (first c) (second c)) c))]))]
    (reverse (lf-helper 0 '(0)))))

(check-expect (list-fibonacci 0)  '(0))
(check-expect (list-fibonacci 1)  '(0 1))
(check-expect (list-fibonacci 2)  '(0 1 1))
(check-expect (list-fibonacci 11) '(0 1 1 2 3 5 8 13 21 34 55 89))

;;; Problem A4

;;; Part 1

;; A WordTree is one of: 
;; -String
;; -(make-node WordTree String WordTree) 

(define-struct node (left word right))

;;; word-in-tree? : WordTree Strring -> Boolean
;;; Is the string in the tree?

(define (word-in-tree? wt str)
  (cond [(equal? wt str) true]
        [(string?    wt) false]
        [(string=? (node-word wt) str) true]
        [else (or (word-in-tree? (node-right wt) str) 
                  (word-in-tree? (node-left  wt) str))]))

(define holidays (make-node 
                  (make-node "Christmas" "Labor Day" "MLK Day")
                  "Patriots Day"
                  (make-node "Presidents Day" "Thanksgiving" "Veterans Day")))
(define movies (make-node 
                (make-node "Argo" "Flight" "Life of Pi")
                "Lincoln"
                "Skyfall"))

(check-expect (word-in-tree? holidays "Patriots Day")   true)
(check-expect (word-in-tree? holidays "Presidents Day") true)
(check-expect (word-in-tree? movies   "Flight")         true)
(check-expect (word-in-tree? movies   "Presidents Day") false)
(check-expect (word-in-tree? holidays "Flight")         false)

;;; Part 2

;;; word-in-tree?.gen : WordTree Strring -> Boolean
;;; Is the string in the tree?

(define (word-in-tree?.gen wt str)
  (cond [(equal? wt str) true]
        [(string?    wt) false]
        [(string=? (node-word wt) str) true]
        [(string>? (node-word wt) str) (word-in-tree?.gen (node-left wt) str)]
        [else (word-in-tree?.gen (node-right wt) str)]))

(check-expect (word-in-tree?.gen holidays "Patriots Day")   true)
(check-expect (word-in-tree?.gen holidays "Presidents Day") true)
(check-expect (word-in-tree?.gen movies   "Flight")         true)
(check-expect (word-in-tree?.gen movies   "Presidents Day") false)
(check-expect (word-in-tree?.gen holidays "Flight")         false)