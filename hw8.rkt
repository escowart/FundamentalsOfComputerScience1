;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; A1
;;; E1

;; An ExamGrade is a (make-grade String String Number)
(define-struct grade (first last points))

#;
(define (ex-temp gr)
  (... (grade-first gr) ... (grade-last gr) ... (grade-points gr) ...))

;; A LOEG (list of exam grades) is one of: 
;; - empty 
;; - (cons ExamGrade LOEG)

#;
(define (loeg-temp loeg)
  (cond[(empty? loeg) ...]
       [else ... (first loeg) ... (rest loeg) ...]))

;;; A LOA is either:
;;; - empty
;;; - (cons Anything LOA)

(define b1 
  (list (make-grade "Seth" "Alexander" 10) 
        (make-grade "Johnny" "Rad" 15)))

(define b2 
  (list (make-grade "Seth" "Alexander" 12) 
        (make-grade "Johnny" "Rad" 8)))

(define b3 
  (list (make-grade "Johnny" "Rad" 8)
        (make-grade "Seth" "Alexander" 12)))

;;; list-length? : LOA LOA -> Boolean
;;; Are the lengths of the two lists equal?
;;; The functions just checks if list are empty at the same time

(define (not-equal-list-length? a b)
  (cond[(or 
         (and (empty? a)
              (not (empty? b)))
         (and (not (empty? a))
              (empty? b))) 
        true]
       [(and (empty? a) (empty? b)) 
        false]
       [else (not-equal-list-length? (rest a) (rest b))]))

(check-expect (not-equal-list-length? empty empty) false)

(check-expect (not-equal-list-length? empty (list 1)) true)

(check-expect (not-equal-list-length? (list 5 4 3 2 1)
                                      (list 1 2 3 4 5)) false)

;;; sum-grades-helper : ExamGrade ExamGrade -> ExamGrade
;;; Sums up the grades a person

(define (sum-grades-helper a b)
  (cond[(and (string=? (grade-first a)
                       (grade-first b))
             (string=? (grade-last  a)
                       (grade-last  b)))
        (make-grade
         (grade-first a)
         (grade-last  a)
         (+ (grade-points a)
            (grade-points b)))]
       [else 
        (error 'sum-grades-helper "The names are not the same")]))

(check-expect (sum-grades-helper (make-grade "Seth" "Alexander" 12) 
                                 (make-grade "Seth" "Alexander" 31))
              (make-grade "Seth" "Alexander" 43))

(check-error (sum-grades-helper (make-grade "Seth"    "Alexander" 12) 
                                (make-grade "NotSeth" "Alexander" 31))
             "sum-grades-helper: The names are not the same")

(check-error (sum-grades-helper (make-grade "Seth" "NotAlexander" 12) 
                                (make-grade "Seth" "Alexander" 31))
             "sum-grades-helper: The names are not the same")

;;; sum-grades-not-error : LOEG LOEG -> LOEG
;;; Sums up the grades of each person 

(define (sum-grades-not-error a b)
  (cond[(empty? a) empty]
       [else (cons (sum-grades-helper    (first a) (first b))
                   (sum-grades-not-error (rest  a) (rest  b)))]))

(check-expect (sum-grades-not-error empty empty)
              empty)

(check-expect (sum-grades-not-error b1 b2) 
              (list (make-grade "Seth" "Alexander" 22) 
                    (make-grade "Johnny" "Rad"     23)))

(check-expect (sum-grades-not-error (append b2 b1) (append b1 b2))
              (list (make-grade "Seth" "Alexander" 22) 
                    (make-grade "Johnny" "Rad"     23)
                    (make-grade "Seth" "Alexander" 22) 
                    (make-grade "Johnny" "Rad"     23)))

;;; sum-grades : LOEG LOEG -> LOEG
;;; Sums up the grades of each person
;;; Each LOEG must be the same length

(define (sum-grades a b)
  (cond[(not-equal-list-length? a b)
        (error 'sum-grades "The input list are not the same length")]
       [else (sum-grades-not-error a b)]))

(check-expect (sum-grades empty empty)
              empty)

(check-error (sum-grades b1 empty)
             "sum-grades: The input list are not the same length")

(check-error (sum-grades empty b1)
             "sum-grades: The input list are not the same length")

(check-expect (sum-grades b1 b2) 
              (list (make-grade "Seth" "Alexander" 22) 
                    (make-grade "Johnny" "Rad"     23)))

(check-error (sum-grades b1 b3)
             "sum-grades-helper: The names are not the same")

(check-expect (sum-grades (append b2 b1) (append b1 b2))
              (list (make-grade "Seth" "Alexander" 22) 
                    (make-grade "Johnny" "Rad"     23)
                    (make-grade "Seth" "Alexander" 22) 
                    (make-grade "Johnny" "Rad"     23)))

;;; E2

(define g1 (list (make-grade "Seth" "Alexander" 10) 
                 (make-grade "Johnny" "Rad" 15)))

(define g2 (list (make-grade "Bob" "Foal" 14) 
                 (make-grade "Patricia" "Klossner" 18) 
                 (make-grade "Radha" "Vijaykumar" 12)))

;;; insert : ExamGrade LOEG -> LOEG
;;; Inserts a ExamGrade alphabetically into a List of ExamGrades

(define (insert loeg eg)
  (cond[(empty? loeg) (list eg)]
       [(string<? 
         (grade-last eg)
         (grade-last (first loeg)))
        (append (list eg) loeg)]
       [else (cons (first loeg)
                   (insert (rest loeg) eg))]))

(check-expect (insert g2 (make-grade "Johnny" "Rad" 15))
              (list 
               (make-grade "Bob"      "Foal"       14) 
               (make-grade "Patricia" "Klossner"   18) 
               (make-grade "Johnny"   "Rad"        15)
               (make-grade "Radha"    "Vijaykumar" 12)))
              
(check-expect (insert empty (make-grade "Johnny" "Rad" 15))
              (list (make-grade "Johnny" "Rad" 15)))

(check-expect (insert g1 (make-grade "Radha" "Vijaykumar" 12))
              (list 
               (make-grade "Seth"   "Alexander"  10) 
               (make-grade "Johnny" "Rad"        15)
               (make-grade "Radha"  "Vijaykumar" 12)))


;;; merge-sorted : LOEG LOEG -> LOEG
;;; Sorts two LOEG by alphabetical order

(define (merge-sorted a b)
  (cond[(empty? a) b]
       [else (merge-sorted (rest a)
                           (insert b (first a)))]))

(check-expect (merge-sorted empty empty)
              empty)

(check-expect (merge-sorted empty b1)
              b1)

(check-expect (merge-sorted g2 g1) 
              (list (make-grade "Seth" "Alexander" 10)
                    (make-grade "Bob" "Foal" 14)
                    (make-grade "Patricia" "Klossner" 18)
                    (make-grade "Johnny" "Rad" 15)
                    (make-grade "Radha" "Vijaykumar" 12)))

(check-expect (merge-sorted g1 g2) 
              (list (make-grade "Seth" "Alexander" 10)
                    (make-grade "Bob" "Foal" 14)
                    (make-grade "Patricia" "Klossner" 18)
                    (make-grade "Johnny" "Rad" 15)
                    (make-grade "Radha" "Vijaykumar" 12)))

;;; A2

;;; E1

 ;; A BTN is one of
 ;; - Number 
 ;; - (make-node BTN BTN) 
(define-struct node (left right))

;; btn-height: BTN -> Number
;; finds the maximum distance between the root of the tree
;; and the furthest leaf 

(define (btn-height a-btn)
  (cond[(number? a-btn) 0]
       [else (add1 (max (btn-height (node-left  a-btn))
                        (btn-height (node-right a-btn))))]))

(check-expect (btn-height 42) 0)
(check-expect (btn-height (make-node 2 (make-node 4 9))) 2) 
(check-expect (btn-height (make-node (make-node 3 (make-node (make-node 3 4) 5))
                                     (make-node 4 9))) 4)

;;; E2

;; btn-sum: BTN -> Number
;; takes the sum of all the leaves in a given BTN


(define (btn-sum a-btn)
  (cond [(number? a-btn) a-btn]
        [else (+ (btn-sum (node-left  a-btn))
                 (btn-sum (node-right a-btn)))]))

(check-expect (btn-sum 42) 42)
(check-expect (btn-sum (make-node 2 (make-node 4 9))) 15)
(check-expect (btn-sum (make-node (make-node 3 (make-node (make-node 3 4) 5)) 
                                  (make-node 4 9))) 28)



