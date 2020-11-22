#lang racket

; cmdline arg with default val
(define in-expr (make-parameter '()))

; cmdline parser
(define parser
    (command-line
         #:args (expr)
         (with-input-from-string expr read)))

; constraint net code
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
  		(if (or (eq? (get-op expr) 'ans) (null? (get-op expr)))	; returning "ans" will do nothing for nil cases
  			'ans
  			(begin (set! count (+ count 1))
  		   	   	   (string->symbol (string-append "l" (number->string count))))))))

(define op-symbol-table #hash((+ . adder)
							  (- . adder)
                              (* . multiplier)
                              (/ . multiplier)
                              (const . constant)))

; future: cond analysis to handle unary, binary, constants
(define (make-constraint-list op sub1-name sub2-name layer-name)
  (list (hash-ref op-symbol-table op) sub1-name sub2-name layer-name))

; constrains right-hand side to left-hand side of eqtn by sharing the top-layer name
(define (get-constraint-list-equaler expr top-layer-name)
  (let ([lhs (get-constraint-list (cadr expr) top-layer-name)]
  		[rhs (get-constraint-list (caddr expr) top-layer-name)])
	(append lhs rhs)))

; build list of constraint code for each layer and its subexpressions/sublayers
; general case (binary op): (op sub1 sub2)
; unary op: 				(op sub1)
; constant:					(op)
(define (get-constraint-list expr current-layer)
  (let* ([op (get-op expr)]			; may be a constant
  		 [sub1 (get-sub1 expr)]
  		 [sub2 (get-sub2 expr)]
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
  		  [(number? op) (list (list 'constant op current-layer))]
  		  [(eq? op 'ans) '()])))	; assume an "ans" by itself is (+ (0) (ans))

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
  (string-append "(begin (define "
                 con-sys-name
                 " (let ("
                 make-connectors
                 ") "
                 (constraint-list->code-string constraint-list)
                 " (get-value ans))) "
                 con-sys-name ")"))

; eval the code and return the answer
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(eval (call-with-input-string (make-constraint-system-code-string "TEST" (get-constraint-list-equaler parser 'top)) read) ns)
