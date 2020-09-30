
    ;; First two lines won't change
    #lang racket
    (require "ConstraintLogic.rkt")
    
    ;; (define (func...
    ;; include a letter for each unknown var
    (define (celsius-fahrenheit-converter c f)
    ;; connector for each layer of calc (parenthesis)
      (let ((u (make-connector))
            (v (make-connector))
            (w (make-connector))
            (x (make-connector))
            (y (make-connector)))
        ;; use operators to define constraints between layers
        (multiplier c w u)
        (multiplier v x u)
        (adder v y f)
        (constant 9 w)
        (constant 5 x)
        (constant 32 y)
        'ok))
    
    ;; make connectors for unknown vars
    (define C (make-connector))
    (define F (make-connector))
    (celsius-fahrenheit-converter C F)
    ;; user must set values for n-1 vars
    (set-value! C 100 'user)
    
    ;; get value for unknown var (nth)
    ;; wrap answer in '[ ]'
    (display (string-append "["
                            (number->string (get-value F))
                            "]"))
    ;; TEST
    