#lang racket

(require "ConstraintSystemBase.rkt")

#|
This program outputs a racket program as a string that represents the full constraint system for execution.
Constraint list is taken as input. This requires output from ConstraintListBuilder.rkt
The Python backend (CalcWrapper) writes the string to a file and runs it using racket to solve for the unknown (ans)

--Example--
input: ((adder l1 l2 ans) (constant a l1) (constant b l2))
output:
; this outputted program is written to a temp file and executed in python using racket
(define generic-system
	(let ((ans (make-connector))
		  (l1 (make-connector))
		  (l2 (make-connector)))
	(adder l1 l2 ans)
	(constant a l1)
	(constant b l2)
	; get answer for the unknown variable
	(get-value ans)))

; this call retrieves the answer
generic-system
|#

; cmdline arg with default val
(define in-expr (make-parameter '()))

; cmdline argument -- constraint list
(define constraint-list
    (command-line #:args (expr) (with-input-from-string expr read)))

; code parser procedures
; grabs the unique connector name symbols from the list of constraint code obtained from get-constraint-list
(define (constraint-list->connector-names constraint-list)
  (define (iter list)
    (cond [(null? list) (set)]
          [(list? (car list)) (set-union (iter (car list)) (iter (cdr list)))]
          [else (list->set (filter symbol? (cdr list)))]))
  (set->list (iter constraint-list)))

; convert given element in constraint list to a string
(define (constraintele->string e)
  (cond [(string? e) e]
  		[(number? e) (number->string e)]
        [(symbol? e) (symbol->string e)]
        ; exception
    	[else "constraint-list->code-string: UNKNOWN ELEMENT"]))

; converts the contraint network to a string able to be 'eval'uated by Racket
(define (constraint-list->code-string constraint-list)
  (substring (foldr string-append
                    ""
                    (map (lambda (c) (string-append " ("
                                                    (substring (foldr string-append
                                                                      ""
                                                                      (map (lambda (e) (string-append " " (constraintele->string e)))
                                                                           c))
                                                               1)
                                                    ")"))
                         constraint-list))
             1))

; Uses a given constraint list to build an entire constraint system that can be executed
(define (make-constraint-system-code-string con-sys-name constraint-list)
  (string? list? . -> . string?)

  (define make-connectors
    [substring (foldr string-append
                      ""
                      (map (lambda (l) (string-append " (" (symbol->string l) " (make-connector))" ))
                           (constraint-list->connector-names constraint-list)))
               1])

  (string-append "(define "
                 con-sys-name
                 " (let ("
                 make-connectors
                 ") "
                 (constraint-list->code-string constraint-list)
                 " (get-value ans))) "
                 con-sys-name))

;; output
(make-constraint-system-code-string "Generic" constraint-list)
