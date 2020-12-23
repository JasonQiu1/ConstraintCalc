#lang racket

(require "ConstraintSystemBase.rkt")

; cmdline arg with default val
(define in-expr (make-parameter '()))

; cmdline argument -- constraint list
(define constraint-list
    (command-line
         #:args (expr)
         (with-input-from-string expr read)))

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

; code parser procedures
; grabs the unique connector name symbols from the list of constraint code obtained from get-constraint-list
(define (constraint-list->connector-names constraint-list)
  (define (iter list)
    (cond [(null? list) (set)]
          [(list? (car list)) (set-union (iter (car list)) (iter (cdr list)))]
          [else (list->set (filter symbol? (cdr list)))]))
  (set->list (iter constraint-list)))

; converts the contraint network to a string able to be 'eval'uated by Racket
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

; parses the entire constraint system together as a string able to be 'eval'uated given a list of constraint relationships
(define (make-constraint-system-code-string con-sys-name constraint-list)
  (string? list? . -> . string?)
  (define make-connectors
    (substring (foldr string-append
                      ""
                      (map (lambda (l) (string-append " (" (symbol->string l) " (make-connector))" ))
                           (constraint-list->connector-names constraint-list)))
               1))
  (string-append "(define "
                 con-sys-name
                 " (let ("
                 make-connectors
                 ") "
                 (constraint-list->code-string constraint-list)
                 " (get-value ans))) "
                 con-sys-name))

(make-constraint-system-code-string "TEST" constraint-list)
