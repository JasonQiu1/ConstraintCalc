; Code adapted and extended from LISP to Racket
; Source (SICP Section 3.3.5): https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3.5

#lang racket

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

; a^b = result
(define powerer (make-binary-constraint (lambda (a b) (expt (get-value a) (get-value b)))
                                           (lambda (a result) (log (get-value result) (get-value a)))
                                           (lambda (b result) (expt (get-value result) (/ 1 (get-value b))))
                                           "POWERER"))


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

(provide (all-defined-out))
