#lang racket
(require "ConstraintSystemBase.rkt")
(define TEST (let ((top (make-connector)) (l4 (make-connector)) (l3 (make-connector)) (l2 (make-connector)) (l5 (make-connector)) (ans (make-connector)) (l1 (make-connector))) (adder l1 l2 top) (multiplier l3 l4 l1) (constant 2 l3) (constant 2 l4) (multiplier ans l5 l2) (constant 2 l5) (constant 5 top) (get-value ans))) TEST