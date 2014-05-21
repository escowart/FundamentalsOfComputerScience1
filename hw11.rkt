;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; --- Problem A1 ---

; a Value is one of: Number String
;
; a Prim is one of:
; '+ '- '* '/ 'sqrt
; 'string-length 'string-append 'number->string
;
; a PExp (primitive expression) is one of: Value PApp
;
; a PApp (primitive application) is a (cons Prim [Listof PExp])

;;;;; Part 1 ;;;;;

; prim?: Any -> Boolean
; is the argument a primitive?
(define (prim? v) 
  (member? v
   '(+ - * / sqrt string-length string-append number->string)))

(check-expect (prim? '+) true)
(check-expect (prim? 'number->string) true)
(check-expect (prim? 'foo) false)


; apply-prim: Prim [Listof Value] -> Value
; apply a primitive to a list of parameter values
; the primitives '+ '- '* '/ and 'string-append accept two or more inputs each
; the remaining primitives accept one input only

(define (apply-prim fn vals)
  (apply
   (cond [(symbol=? '+             fn)             +]
         [(symbol=? '-             fn)             -]
         [(symbol=? '*             fn)             *]
         [(symbol=? '/             fn)             /]
         [(symbol=? 'sqrt          fn)          sqrt]
         [(symbol=? 'string-length fn) string-length]
         [(symbol=? 'string-append fn) string-append]
         [else number->string])
     vals))

(check-expect (apply-prim '+              '(1 2 3)) 6)
(check-expect (apply-prim '-              '(1 2 3)) -4)
(check-expect (apply-prim '*              '(1 2 3)) 6)
(check-expect (apply-prim '/              '(1 2)) (/ 1 2))
(check-expect (apply-prim 'sqrt           '(4)) 2)
(check-expect (apply-prim 'string-length  '("foo")) 3)
(check-expect (apply-prim 'string-append  '("a" "b")) "ab")
(check-expect (apply-prim 'number->string '(23)) "23")

;;;;; Part 2 ;;;;;

;;; numstr? : Any -> Boolean 
;;; Is the input a number or string?

(define (numstr? a)
  (or (number? a) (string? a)))

; prim-eval: PExp -> Value
; evaluate a primitive expression
(define (prim-eval s) 
  (cond [(numstr? s) s]
        [(empty? s) empty]
        [(cons? (first s)) (cons (prim-eval (first s)) (prim-eval (rest s)))]
        [(symbol? (first s)) (apply-prim (first s) (prim-eval (rest s)))]
        [else (cons (first s) (prim-eval (rest s)))]))

(check-expect (prim-eval 2) 2)
(check-expect (prim-eval "a") "a")
(check-expect (prim-eval '(+ 3 (* 2 3) 5)) 14)
(check-expect (prim-eval '(string-append "a" (number->string 2))) "a2")
(check-expect (prim-eval '(* (string-length "bob") 2)) 6)

;;; --- Problem A2 ---

; a Name is a Symbol
;
; a Func is one of: Prim Name
;
; a SExp (simple scheme expression) is one of: Value Name App
;
; an App (application) is a (cons Func [Listof SExp])
;
; a Def (definition) is a (make-def Name [Listof Name] SExp)
(define-struct def (name args body))

(define the-defs (list (make-def 'pi '() 3.14)
                       (make-def 'length '(x y) '(sqrt (+ (* x x) (* y y))))
                       (make-def 'circumference '(r) '(* 2 pi r))
                       (make-def 'prepend-bob '(s) '(string-append "bob" s))))

;;;;; Part 1 ;;;;;

; lookup: Name Number [Listof Def] -> Def
; find the definition with the given name and number of args in defs
; or (error name " not defined")
(define (lookup name nargs defs) 
  (cond [(empty? defs) (error name " not defined")]
        [(and (symbol=? (def-name (first defs)) name)
              (= (length (def-args (first defs))) nargs))
         (first defs)]
        [else (lookup name nargs (rest defs))]))

(check-expect (def-body (lookup 'pi 0 the-defs)) 3.14)
(check-expect (def? (lookup 'length 2 the-defs)) true)
(check-error (lookup 'length 3 the-defs))
(check-error (lookup 'sqr 1 the-defs))

;;;;; Part 2 ;;;;;

; subst: [Listof Value] [Listof Name] SExp -> SExp
; substitute the corresponding value for each listed variable name in s
; leave all other names in s unmodified
(define (subst vals vars s)
  (cond [(not (= (length vals) (length vars)))
         (error 'subst "Value and Name List are different lengths")]
        [(empty? vars) s]
        [(cons? s) 
         (map (λ (e) (subst vals vars e)) s)]
        [(equal? s (first vars))
         (first vals)]
        [else (subst (rest vals) (rest vars) s)]))
       
(define s1 '(foo a 29 (bar "z" b)))
(check-expect (subst '(3 "x") '(a b) s1) '(foo 3 29 (bar "z" "x")))

;;;;; Part 3 ;;;;;

;;; find-def : Name [Listof Def] -> Def
;;; Finds the corresponding definition
(define (find-def fn d)
  (cond [(empty? d) (error 'my-apply "da function is not defined")]
        [(symbol=? (def-name (first d)) fn) (first d)]
        [else (find-def fn (rest d))]))

;;; function-with-no-par? : Any  [Listof Def] -> Boolean 
;;; Does the given function have no parameter?

(define (function-with-no-par? a defs)
  (and (symbol? a); blocks anything that is not a symbol
       (empty? (def-args (find-def a defs)))))

;;; my-apply : Func [Listof Value] [Listof Def] -> Value
;;; apply either a primitive or a defined function to a list of parameter values

(define (my-apply fn vals defs)
  (local [(define prim-or-def
            (cond [(prim? fn) (cons fn vals)]
                  [else (subst vals 
                               (def-args (find-def fn defs)) 
                               (def-body (find-def fn defs)))]))]
    (my-eval prim-or-def defs)))


;;; my-eval : SExp [Listof Def] -> Value
;;; evaluate a scheme expression given a list of definitions

(define (my-eval s defs)
  (cond [(numstr? s) s]
        [(symbol? s) (my-apply s empty defs)]
        [(empty? s) empty]
        [(cons? (first s)) 
         (cons (my-eval (first s) defs) (my-eval (rest s) defs))]
        [(prim?   (first s)) (apply-prim (first s) (my-eval (rest s) defs))]
        [(numstr? (first s)) (cons (first s)  (my-eval (rest s) defs))]
        [(function-with-no-par? (first s) defs) 
         (cons (my-apply (first s) empty defs) (my-eval (rest s) defs))]
        [else (my-apply (first s) (my-eval (rest s) defs) defs)]))



(check-expect (my-apply '+ '(1 2 3) the-defs) 6)
(check-expect (my-apply 'length '(3 4) the-defs) 5)
(check-error (my-apply 'sqr '(2) the-defs))
(check-error (my-apply 'circumference '(1 2) the-defs))

(check-expect (my-eval 2 the-defs) 2)
(check-expect (my-eval "b" the-defs) "b")
(check-expect (my-eval '(+ 3 4) the-defs) 7)
(check-expect (my-eval 'pi the-defs) 3.14)
(check-expect (my-eval '(* 2 pi) the-defs) 6.28)
(check-expect (my-eval '(prepend-bob (number->string 18)) the-defs) "bob18")
(check-expect (my-eval '(circumference 2) the-defs) (* 4 3.14))
(check-expect 
 (my-eval '(circumference (/ 10 (length 3 4))) the-defs) (* 4 3.14))
(check-expect 
 (my-eval '(* (string-length (string-append "a" "b")) 4) the-defs) 8)
(check-error  (my-eval '(sqr 4) the-defs))
(check-expect 
 (my-eval '(sqr 4) (cons (make-def 'sqr '(x) '(* x x)) the-defs)) 16)