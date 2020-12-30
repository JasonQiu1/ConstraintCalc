#lang racket
(require "ConstraintSystemBase.rkt")
(define TEST (let ((top (make-connector)) (l3 (make-connector)) (l2 (make-connector)) (ans (make-connector)) (l1 (make-connector))) (adder l1 l2 top) (constant 2 l1) (constant 2 l2) (adder ans l3 top) (constant 0 l3) (get-value ans))) TEST