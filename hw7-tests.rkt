#lang racket450/testing

; Pull in all the functions we wrote in the main file
(require "hw7.rkt")

;; --- Tests for mk-array ---
(check-equal? (mk-array '[1 2 3]) '(1 2 3))
(check-equal? (mk-array '[[1 2] [3 4]]) '((1 2) (3 4)))

;; --- Tests for ndim ---
(check-equal? (ndim 5) 0)
(check-equal? (ndim (mk-array '[])) 1)
(check-equal? (ndim (mk-array '[1 2 3])) 1)
(check-equal? (ndim (mk-array '[[1 2] [3 4]])) 2)

;; --- Tests for shape ---
(check-equal? (shape 5) '())
(check-equal? (shape (mk-array '[])) '(0))
(check-equal? (shape (mk-array '[1 2 3])) '(3))
(check-equal? (shape (mk-array '[[1 2 3] [4 5 6]])) '(2 3))

;; --- Tests for pos-index (Helper) ---
(check-equal? (pos-index 2 5) 2)
(check-equal? (pos-index -1 5) 4)
(check-equal? (pos-index -2 5) 3)

;; --- Tests for ref ---
(check-equal? (ref (mk-array '[10 20 30]) '(1)) 20)
(check-equal? (ref (mk-array '[10 20 30]) '(-1)) 30)
(check-equal? (ref (mk-array '[[1 2] [3 4]]) '(1 0)) 3)
(check-equal? (ref (mk-array '[[1 2] [3 4]]) '(0)) '(1 2))

;; --- Tests for filter/step ---
(check-equal? (filter/step '(10 20 30 40 50) 1) '(10 20 30 40 50))
(check-equal? (filter/step '(10 20 30 40 50) 2) '(10 30 50))
(check-equal? (filter/step '(10 20 30 40 50) 3) '(10 40))

;; --- Tests for slice1 ---
(check-equal? (slice1 (mk-array '[10 20 30 40 50]) 1) 20)
(check-equal? (slice1 (mk-array '[10 20 30 40 50]) -1) 50)
; Testing with the compound slice struct using our API function
(check-equal? (slice1 (mk-array '[10 20 30 40 50]) 
                      (mk-Slice/testing #:start 1 #:stop 4 #:step 2)) 
              '(20 40))

