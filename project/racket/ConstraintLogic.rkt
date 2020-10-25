#lang racket
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (unless (memq new-constraint constraints)
          (set! constraints 
                (cons new-constraint constraints)))
      (when (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (make-unary-constraint has-operand-proc has-result-proc constraint-name)
  (lambda (operand result)
    (define (process-new-value)
      (cond [(has-value? operand)
             (set-value! result
                         (has-operand-proc operand)
                         me)]
            [(has-value? result)
             (set-value! operand
                         (has-result-proc result)
                         me)]))
    (define (process-forget-value)
      (forget-value! result me)
      (forget-value! operand me)
      (process-new-value))
    (define (me request)
      (cond ((eq? request 'I-have-a-value)  
             (process-new-value))
            ((eq? request 'I-lost-my-value) 
             (process-forget-value))
            (else 
             (error (string-append "Unknown request -- " constraint-name) request))))
    (connect operand me)
    (connect result me)
    me))

(define (make-binary-constraint has-both-proc has-first-proc has-second-proc constraint-name)
  (lambda (operand1 operand2 result)
    (define (process-new-value)
      (cond ((and (has-value? operand1) (has-value? operand2))
             (set-value! result
                         (has-both-proc operand1 operand2)
                         me))
            ((and (has-value? operand1) (has-value? result))
             (set-value! operand2
                         (has-first-proc operand1 result)
                         me))
            ((and (has-value? operand2) (has-value? result))
             (set-value! operand1
                         (has-second-proc operand2 result)
                         me))))
    (define (process-forget-value)
      (forget-value! result me)
      (forget-value! operand1 me)
      (forget-value! operand2 me)
      (process-new-value))
    (define (me request)
      (cond ((eq? request 'I-have-a-value)  
             (process-new-value))
            ((eq? request 'I-lost-my-value) 
             (process-forget-value))
            (else 
             (error (string-append "Unknown request -- " constraint-name) request))))
    (connect operand1 me)
    (connect operand2 me)
    (connect result me)
    me))
  
(define adder (make-binary-constraint (lambda (a1 a2) (+ (get-value a1) (get-value a2)))
                                      (lambda (a1 sum) (- (get-value sum) (get-value a1)))
                                      (lambda (a2 sum) (- (get-value sum) (get-value a2)))
                                      "ADDER"))

(define multiplier (make-binary-constraint (lambda (m1 m2) (* (get-value m1) (get-value m2)))
                                           (lambda (m1 product) (/ (get-value product) (get-value m1)))
                                           (lambda (m2 product) (/ (get-value product) (get-value m2)))
                                           "MULTIPLIER"))

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

;; accessors for prefix expressions
(define (get-op expr)
  (if (> (length expr) 0)
      (car expr)
      '()))
	
(define (get-sub1 expr)
  (if (> (length expr) 1)
      (cadr expr)
      '()))
	
(define (get-sub2 expr)
  (if (> (length expr) 2)
      (caddr expr)
      '()))
	
; return unused name or "ans" if "ans" is found in expr
(define next-list-name
  (let ([count 0])
    (lambda (expr)
      (if (or (eq? (get-op expr) "ans") (null? (get-op expr)))	; returning "ans" will do nothing for nil cases
          "ans"
          (begin (set! count (+ count 1))
                 (string-append "l" (number->string count)))))))
	
(define op-symbol-table #hash(("+" . "adder")
                              ("-" . "adder")
                              ("*" . "multiplier")
                              ("const" . "constant")))
	
; future: cond analysis to handle unary, binary, constants
(define (make-constraint-list op sub1-name sub2-name layer-name)
  (list (hash-ref op-symbol-table op) sub1-name sub2-name layer-name))
	
;;(define (get-constraint-code expr names)
;;  	(cond 	[(eq? (car expr) '+) (append (list (list 'adder (cadr names) (caddr names) layer-name)) (list (get-constraint-code (cadr expr) (cadr names) (cdr names)))) (list (get-constraint-code (caddr expr) (car names) (cdr names))))]
;;			[else (list 'constant (car expr) layer-name)]))
	
; constrains right-hand side to left-hand side of eqtn by sharing the top-layer name
(define (get-constraint-code-equaler expr top-layer-name)
  (let ([lhs (get-constraint-code (cadr expr) top-layer-name)]
        [rhs (get-constraint-code (caddr expr) top-layer-name)])
    (append lhs rhs)))
	
; build list of constraint code for each layer and its subexpressions/sublayers
; general case (binary op): (op sub1 sub2)
; unary op: 				(op sub1)
; constant:					(op)
(define (get-constraint-code expr current-layer)
  (let* ([op (get-op expr)]			; may be a constant
         [sub1 (get-sub1 expr)]
         [sub2 (get-sub2 expr)]
         [sub1-name (next-list-name sub1)]
         [sub2-name (next-list-name sub2)])
    (cond [(eq? op "+") (append (list (make-constraint-list op sub1-name sub2-name current-layer))
                                (get-constraint-code sub1 sub1-name)
                                (get-constraint-code sub2 sub2-name))]
          [(eq? op "-") (append (list (make-constraint-list op current-layer sub2-name sub1-name))
                                (get-constraint-code sub1 sub1-name)
                                (get-constraint-code sub2 sub2-name))]
          [(number? op) (list (list "constant" op current-layer))]
          [(eq? op "ans") '()])))	; assume an "ans" by itself is (+ (0) (ans))

(define (constraint-list->connector-name-list constraint-list)
  (define (iter list)
    (cond [(or (null? list)) (set)]
          [(list? (car list)) (set-union (iter (car list)) (iter (cdr list)))]
          [else (list->set (filter string? list))]))
  (set->list (iter constraint-list)))

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

(define (make-constraint-system-code-string con-sys-name connector-name-list constraint-code-string)
  (define make-connectors
    (substring (foldr string-append
                      ""
                      (map (lambda (l) (string-append " (" l " (make-connector))" ))
                           connector-name-list))
               1))
  (string-append "(define "
                 con-sys-name
                 " (let ("
                 make-connectors
                 ") "
                 constraint-code-string
                 " (get-value ans)))"))

(define constraint-table #hash(("+" . 'adder)
                               ("*" . 'multiplier)
                               ("=" . 'equaler)))
