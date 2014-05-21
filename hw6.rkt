;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ps6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; Problem A1

;;; 1.

;; A family-tree-node is one of: 
;; - empty 
;; - (make-child father mother name date hair-color) 
;; where father and mother are family-tree-nodes, 
;; name and hair-color are symbols, and date is a number.

(define-struct child (father mother name date hair-color))

#;
(define (ftn-temp ftn)
  (cond[(empty? ftn) ...]
       [else ... (ftn-temp (child-father ftn)) ... 
             ... (ftn-temp (child-mother ftn)) ... 
             ... (child-name ftn)              ... 
             ... (child-date ftn)              ...  
             ... (child-hair-color ftn)        ...  ]))

;;; count-older : family-tree-node Number -> Number
;;; Returns how many people in the family tree 
;;; that were born that year or before

(define (count-older ftn year)
  (cond[(empty? ftn) 0]
       [(>= (child-date ftn) year)
        (+ 1
         (count-older (child-father ftn) year)
         (count-older (child-mother ftn) year))]
       [else (+ 
         (count-older (child-father ftn) year)
         (count-older (child-mother ftn) year))]))

(define c1 (make-child empty empty 'f 10 'red)) 
(define c2 (make-child empty empty 'm 10 'black)) 
(define c3 (make-child c1     c2 'name 1 'black))
(define c4 (make-child c1     c2 'name 3 'red))
(define c5 (make-child c3     c4 'name 3 'black))
(define c6 (make-child c3     c3 'name 3 'black))
(define c7 (make-child c2     c3 'name 3 'black))


(check-expect (count-older c3 2) 2)
(check-expect (count-older empty 2) 0)

;;; 2.

;;; A LoSym is ether:
;;; - empty
;;; - (cons Symbol LoSym)

#;
(define (LoSym-temp ls)
  (cond[(empty? ls) ...]
       [else ... (first ls) ... (LoSym-temp (rest ls))]))

;;; list-append-ft : List List -> List
;;; Adds the second list to the end of the first list if the first is LoSym
;;; otherwise it just adds the first to the second
;;; the second arguement is a LoSym

(define (list-append l1 l2)
  (cond[(symbol? l1) (cons l1 l2)]
       [(empty? l1) l2]
       [else (cons (first l1) (list-append (rest l1) l2))]))

(check-expect (list-append 'l1 (list 'l2)) (list 'l1 'l2))
(check-expect (list-append 'l1 (list 'l2 'l3)) (list 'l1 'l2 'l3))
(check-expect (list-append (list 'l1 'l2) (list 'l3)) (list 'l1 'l2 'l3))

;;; search-tree-older : family-tree-node Number -> LoSym
;;; Returns a LoSym with the names of people in the family tree 
;;; that were born that year or before

(define (search-tree-older ftn year)
  (cond[(empty? ftn) empty]
       [(>= (child-date ftn) year)
        (list-append (child-name ftn) 
        (list-append (search-tree-older (child-father ftn) year)
                     (search-tree-older (child-mother ftn) year)))]
       [else (list-append
         (search-tree-older (child-father ftn) year)
         (search-tree-older (child-mother ftn) year))]))



(check-expect (search-tree-older c5 1) (list 'name 'name 'f 'm 'name 'f 'm))
(check-expect (search-tree-older c5 9) (list 'f 'm 'f 'm))

;;; 3.

;;; red-haired-or-ancestors? : family-tree-node -> Boolean
;;; Test whether a has red hair or their ancestor does

(define (red-haired-or-ancestors? ftn)
  (cond[(empty? ftn) false]
       [(symbol=? (child-hair-color ftn) 'red) true]
       [else 
        (or (red-haired-or-ancestors? (child-father ftn))
            (red-haired-or-ancestors? (child-mother ftn)))]))
       
(check-expect (red-haired-or-ancestors? c2) false)
(check-expect (red-haired-or-ancestors? c3) true)
(check-expect (red-haired-or-ancestors? c4) true)

;;; red-haired-ancestors? : family-tree-node -> Boolean
;;; Tests if an ancestor on the both sides of the tree have red hair

(define (red-haired-ancestors? ftn)
  (and (red-haired-or-ancestors? (child-father ftn)) 
       (red-haired-or-ancestors? (child-mother ftn))))

(check-expect (red-haired-ancestors? c7) false)
(check-expect (red-haired-ancestors? c5) true)
(check-expect (red-haired-ancestors? c6) true)

;;; 4.

;;; update-father : family-tree-node family-tree-node -> family-tree-node
;;; Updates the father of the first tree to be second tree input

(define (update-father child father)
  (make-child                   father 
              (child-mother     child) 
              (child-name       child) 
              (child-date       child) 
              (child-hair-color child)))

(check-expect (update-father c6 c2) (make-child c2 c3 'name 3 'black))


;;; Problem A2

;;; a LoXP is either:
;;; - empty
;;; - (cons Number LoXP)

#;
(define (LoXP-temp lxp)
  (cond[(empty? lxp) ...]
       [else ... (first lxp) ... (LoXP-temp (rest lxp))]))

;;; a LoYP is either:
;;; - empty
;;; - (cons Number LoYP)

#;
(define (LoYP-temp lyp)
  (cond[(empty? lyp) ...]
       [else ... (first lyp) ... (LoYP-temp (rest lyp))]))

;;; a LoP is either:
;;; - empty
;;; - (cons Posn LoP)

#;
(define (LoP-temp lp)
  (cond[(empty? lp) ...]
       [else ... (first lp) ... (LoP-temp (rest lp))]))

;;; make-points : LoXP LoYP -> LoP
;;; Combines a LoXP and LoYPinto a LoP
;;; LoXP and LoYP must be the same length

(define (make-points lxp lyp)
  (cond[(or (empty? lxp) (empty? lyp)) empty]
       [else (cons (make-posn (first lxp) (first lyp))
                   (make-points (rest lxp) (rest lyp)))]))

(check-expect (make-points (list 1 2) (list 3 4)) 
              (list (make-posn 1 3) (make-posn 2 4)))

;;; make-1stQpoints : LoXP LoYP -> LoP
;;; Combines a LoXP and LoYPinto a LoP only if they are in Q1 of the 
;;; LoXP and LoYP must be the same length

(define (make-1stQpoints lxp lyp)
  (cond[(or (empty? lxp) (empty? lyp)) empty]
       [(and (>= (first lxp) 0) (>= (first lyp) 0))
        (cons (make-posn (first lxp) (first lyp))
              (make-1stQpoints (rest lxp) (rest lyp)))]
       [else  (make-1stQpoints (rest lxp) (rest lyp))]))

(check-expect (make-1stQpoints (list -5 1 2 -4 4 -5) (list 5 3 4 4 -4 -5)) 
              (list (make-posn 1 3) (make-posn 2 4)))

;;; Problem A3

;; An Xexpr is one of: 
;;  - (cons Symbol LoXexpr)
;;  - String 

;; An LoXexpr is one of: 
;;  - empty 
;;  - (cons Xexpr LoXexpr)

#|
<div>
       <p><b>Problem A1</b>:</p>
  
       <blockquote>
         <p>

           XML is a modern language (really, a
           <i>family</i> of languages) for encoding
           data. It uses "<i>named parentheses</i>" to
           group structured data and strings for everything
           else. [Well, it contains a few more things than that, but
           this is good enough for now.] Here is an example:

         </p> 

         <p> Yes, this example is weird, because it is 
           self-referential but, hey, you should be used to
           this by now.  
         </p>
       </blockquote>
     </div>
|#
