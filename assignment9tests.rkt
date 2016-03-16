#lang racket
;; Programming Languages, Assignment 9 tests
;;
;; This file uses a basic testing mechanism similar to what we did with OCAML.
;; To test, simply run this file in DrRacket.

(require "assignment9sub.rkt")

;; add-nums
(equal? (add-nums (list)) 0) ;;empty list
(equal? (add-nums (list 1 2 'a 3)) 6) ;;non-number

;; length
(equal? (length (list)) 0) ;; empty list
(equal? (length (list 1 2 3)) 3) ;; regular list

;; get-nth
(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "negative index"))])
    (get-nth null -2))   ;;negative index
(equal? (get-nth (list 1 2 3) 2) 3)
(equal? (get-nth (list 1 2 3) 0) 1)

;; every-other
(equal? (every-other (list 1 2 3 4)) (list 1 3)) ;; even length
(equal? (every-other (list 1 2 3)) (list 1 3))   ;; odd length
(equal? (every-other (list)) (list)) ;; empty list

;; map
(equal? (map (lambda (x) (* x x)) (list 1 2 3))
     (list 1 4 9))       ;; squaring
(equal? (map (lambda (x) (* x x)) (list))
     (list))       ;; empty list
(equal? (map (lambda (x) (+ x x)) (list 1 2 3))
     (list 2 4 6))       ;; addition

;; map2
(equal? (map2 (lambda (x y) (* x y)) (list 1 2 3) (list 2 3 4))
     (list 2 6 12))      ;; multiply
(equal? (map2 (lambda (x y) (* x y)) (list 1 2 3) (list 2))
     (list 2))      ;; list 1 smaller
(equal? (map2 (lambda (x y) (* x y)) (list 1) (list 2 3 4))
     (list 2))      ;; list 2 smaller
(equal? (map2 (lambda (x y) (* x y)) (list) (list 2 3 4))
     (list))      ;; one list empty

;; filter
(equal? (filter (lambda (x) (= (modulo x 2) 1))
               (list 1 2 3 4))
     (list 1 3))      ;; odd
(equal? (filter (lambda (x) (= (modulo x 2) 0))
               (list 1 2 3 4))
     (list 2 4))      ;; even
(equal? (filter (lambda (x) (= (modulo x 2) 0))
               (list 1 3))
     (list))      ;; all false
(equal? (filter (lambda (x) (= (modulo x 2) 0))
               (list))
     (list))      ;; empty list

;; call-all
(equal? (call-all (list (lambda () 2)))
     (list 2))        ;; one-element
(equal? (call-all (list (lambda () 2) (lambda () 5)))
     (list 2 5))        ;; two-element
(equal? (call-all (list))
     (list))        ;; zero-elements