#lang racket
(require "ConstraintSystemBase.rkt")
(define TEST (let ((top (make-connector)) (l3 (make-connector)) (l2 (make-connector)) (ans (make-connector)) (l1 (make-connector))) (powerer top l2 l1) (constant 4 l1) (constant 2 l2) (adder l3 ans top) (constant 0 l3) (get-value ans))) TEST