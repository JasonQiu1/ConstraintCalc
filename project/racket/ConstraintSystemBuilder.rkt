#lang racket

(require "ConstraintSystemBase.rkt")

; cmdline arg with default val
(define in-expr (make-parameter '()))

; cmdline parser
(define parser
    (command-line
         #:args (expr)
         (with-input-from-string expr read)))

; accessors for prefix expressions
; expr: a list
; len-guard: which element # you want to access. allows for safe access
; len-guard ex: if the size of the list is 1, and len-guard is 1, this will safely access the 1st element
(define (safe-list-ref expr len-guard)
  (if (and (list? expr) (>= (length expr) len-guard))
  	  (list-ref expr (- len-guard 1)) ; len-guard -1 is the element we want to get
  	  '())) ; return nil if access was invalid

; return unused name or "ans" if "ans" is found in expr
(define next-list-name
  (let ([count 0])
  	(lambda (expr)
  		(if (or (eq? (safe-list-ref expr 1) 'ans) (null? (safe-list-ref expr 1)))	; returning "ans" will do nothing for nil cases
  			'ans
  			(begin (set! count (+ count 1))
  		   	   	   (string->symbol (string-append "l" (number->string count))))))))

(define op-symbol-table #hash((+ . adder)
							  (- . adder)
                              (* . multiplier)
                              (/ . multiplier)
                              (^ . powerer)
                              (log . powerer)
                              (const . constant)))

; future: cond analysis to handle unary, binary, constants
(define (make-constraint-list op sub1-name sub2-name layer-name)
  (list (hash-ref op-symbol-table op) sub1-name sub2-name layer-name))

; constrains right-hand side to left-hand side of eqtn by sharing the top-layer name
(define (get-constraint-list-equaler expr top-layer-name)
  (let ([lhs (cadr expr)]
  		[rhs (caddr expr)])
  	  (if (or (eq? 'ans (car lhs)) (eq? 'ans (car rhs))) ; if 'ans is by itself on the right or left
  	  	  (append
  	  		(get-constraint-list lhs 'ans)
  	  		(get-constraint-list rhs 'ans))
  	  	  (append
  	  		(get-constraint-list lhs top-layer-name)
  	  		(get-constraint-list rhs top-layer-name)))))

; build list of constraint code for each layer and its subexpressions/sublayers
; general case (binary op): (op sub1 sub2)
; unary op: 				(op sub1)
; constant:					(op)
(define (get-constraint-list expr current-layer)
  (let* ([op (safe-list-ref expr 1)]			; may be a constant (symbol or num)
  		 [sub1 (safe-list-ref expr 2)]
  		 [sub2 (safe-list-ref expr 3)]
  		 [op-name (next-list-name op)]
  		 [sub1-name (next-list-name sub1)]
  		 [sub2-name (next-list-name sub2)])
  	(cond [(eq? op '+) (append (list (make-constraint-list op sub1-name sub2-name current-layer))
  								(get-constraint-list sub1 sub1-name)
  								(get-constraint-list sub2 sub2-name))]
  		  [(eq? op '-) (append (list (make-constraint-list op current-layer sub2-name sub1-name))
  								(get-constraint-list sub1 sub1-name)
  								(get-constraint-list sub2 sub2-name))]
 	 	  [(eq? op '*) (append (list (make-constraint-list op sub1-name sub2-name current-layer))
  								(get-constraint-list sub1 sub1-name)
  								(get-constraint-list sub2 sub2-name))]
  		  [(eq? op '/) (append (list (make-constraint-list op current-layer sub2-name sub1-name))
  								(get-constraint-list sub1 sub1-name)
  								(get-constraint-list sub2 sub2-name))]
 	 	  [(eq? op '^) (append (list (make-constraint-list op sub1-name sub2-name current-layer))
  								(get-constraint-list sub1 sub1-name)
  								(get-constraint-list sub2 sub2-name))]
  		  [(eq? op 'log) (append (list (make-constraint-list op current-layer sub2-name sub1-name))
  								(get-constraint-list sub1 sub1-name)
  								(get-constraint-list sub2 sub2-name))]
		  [(eq? op 'ans) '()]	; assume an "ans" by itself is (+ (0) (ans))
  		  [else (list (list 'constant op current-layer))])))

; code parser procedures
; grabs the unique connector name symbols from the list of constraint code obtained from get-constraint-list
(define (constraint-list->connector-name-list constraint-list)
  (define (iter list)
    (cond [(null? list) (set)]
          [(list? (car list)) (set-union (iter (car list)) (iter (cdr list)))]
          [else (list->set (filter symbol? (cdr list)))]))
  (set->list (iter constraint-list)))

; converts the list of constraint code to a string able to be 'eval'uated
(define (constraint-list->code-string constraint-list)
  (substring (foldr string-append
                    ""
                    (map (lambda (c) (string-append " ("
                                                    (substring (foldr string-append
                                                                      ""
                                                                      (map (lambda (e) (string-append " " (cond [(string? e) e]
                                                                                                                [(number? e) (number->string e)]
                                                                                                                [(symbol? e) (symbol->string e)]
                                                                                                                [else "constraint-list->code-string: UNKNOWN ELEMENT"])))
                                                                           c))
                                                               1)
                                                    ")"))
                         constraint-list))
             1))

;; (define generic-constraint-system
;;   (let ((ans (make-connector))
;;   		(l1 (make-connector))
;;         (l2 (make-connector))
;;         (l3 (make-connector))
;;         (l4 (make-connector))
;;         (l5 (make-connector))
;;         (l6 (make-connector)))
;;     (multiplier l6 l3 l1)
;;     (multiplier l2 l4 l1)
;;     (adder l2 l5 ans)
;;     (constant 9 l3)
;;     (constant 32 l5)
;;     (constant 5 l4)
;;     (constant 100.888 l6)
;;     (get-value ans)))
;;
;; generic-constraint-system

; parses the entire constraint system together as a string able to be 'eval'uated given a list of constraint relationships
(define (make-constraint-system-code-string con-sys-name constraint-list)
  (string? list? . -> . string?)
  (define make-connectors
    (substring (foldr string-append
                      ""
                      (map (lambda (l) (string-append " (" (symbol->string l) " (make-connector))" ))
                           (constraint-list->connector-name-list constraint-list)))
               1))
  (string-append "(define "
                 con-sys-name
                 " (let ("
                 make-connectors
                 ") "
                 (constraint-list->code-string constraint-list)
                 " (get-value ans))) "
                 con-sys-name))

; return the parsed constraint system
(make-constraint-system-code-string "TEST" (get-constraint-list-equaler parser 'top))
