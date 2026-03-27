#lang racket450

; An Array is one of:
; - Number
; - ArrayList
; Invariant: Must be "rectangular" (along any one dimension, each element must have the same length).

; An ArrayList is one of:
; - empty
; - (cons Array ArrayList)

; An Index is one of:
; - a nonnegative Int
; - a negative Int
; Represents the (0-based) n-th element in an Array. -1 is the last element.
; A Slice is one of:
; - Index
; - (slice Index Index Number)
(struct slice (start stop step) #:transparent)

; mk-array : Array -> Array
; Constructor that returns the given array argument unchanged.
; Examples:
; (mk-array '[1 2 3]) => '(1 2 3)
; (mk-array '[[1 2] [3 4]]) => '((1 2) (3 4))

(define (mk-array arr)
  arr)
; ndim : Array -> exact-nonnegative-integer?
; Computes the number of dimensions (nestedness) of the given Array.
; Examples:
; (ndim 5) => 0
; (ndim (mk-array '[])) => 1
; (ndim (mk-array '[1 2 3])) => 1
; (ndim (mk-array '[[1 2] [3 4]])) => 2
(define (ndim arr)
  (cond
    [(number? arr) 0]
    [(empty? arr) 1]
    [(list? arr) (+ 1 (ndim (first arr)))]))
; shape : Array -> (listof exact-nonnegative-integer?)
; Computes the length of the given Array along each of its dimensions.
; Examples:
; (shape 5) => '()
; (shape (mk-array '[])) => '(0)
; (shape (mk-array '[1 2 3])) => '(3)
; (shape (mk-array '[[1 2 3] [4 5 6]])) => '(2 3)
(define (shape arr)
  (cond
    [(number? arr) empty]
    [(empty? arr) (list 0)]
    [(list? arr) (cons (length arr) (shape (first arr)))]))
; pos-index : Index exact-nonnegative-integer? -> exact-nonnegative-integer?
; Helper function: Converts a potentially negative index to a normal positive 0-based index.
; Examples:
; (pos-index 2 5) => 2
; (pos-index -1 5) => 4
(define (pos-index i len)
  (if (< i 0)
      (+ len i)
      i))


; ref : Array (listof Index) -> Array
; Returns the element or sub-array in the given Array referenced by the list of indices.
; Examples:
; (ref (mk-array '[10 20 30]) '(1)) => 20
; (ref (mk-array '[10 20 30]) '(-1)) => 30
; (ref (mk-array '[[1 2] [3 4]]) '(1 0)) => 3
; (ref (mk-array '[[1 2] [3 4]]) '(0)) => '(1 2)
(define (ref arr indices)
  (cond
    ; If we run out of indices, we have arrived at the target element/sub-array
    [(empty? indices) arr]
    
    ; Otherwise, find the positive index, extract that sub-array, and keep searching
    [else 
     (let* ([current-index (first indices)]
            [arr-length (length arr)]
            [positive-index (pos-index current-index arr-length)]
            [sub-array (list-ref arr positive-index)])
       
       (ref sub-array (rest indices)))]))
; filter/step : list? exact-nonnegative-integer? -> list?
; Returns the elements of the input list, keeping one element and then skipping elements based on the step size.
; Examples:
; (filter/step '(10 20 30 40 50) 1) => '(10 20 30 40 50)
; (filter/step '(10 20 30 40 50) 2) => '(10 30 50)
; (filter/step '(10 20 30 40 50) 3) => '(10 40)

(define (filter/step lst step)
  ; Helper function that uses an accumulator (skip-count)
  (define (filter-acc remaining skip-count)
    (cond
      [(empty? remaining) empty]
      ; When skip-count hits 0, we keep the item and reset the count to (step - 1)
      [(= skip-count 0) 
       (cons (first remaining) (filter-acc (rest remaining) (- step 1)))]
      ; Otherwise, we skip the item and count down by 1
      [else 
       (filter-acc (rest remaining) (- skip-count 1))]))
  
  ; We kick off the helper function starting with a skip-count of 0 (so we always keep the first item)
  (filter-acc lst 0))
; slice1 : ArrayList Slice -> Array
; Outputs the elements of the given Array specified by the given Slice for the outermost dimension.
; Examples:
; (slice1 (mk-array '[10 20 30 40 50]) 1) => 20
; (slice1 (mk-array '[10 20 30 40 50]) -1) => 50
; (slice1 (mk-array '[10 20 30 40 50]) (slice 1 4 2)) => '(20 40)

(define (slice1 arr slc)
  (cond
    ; Case 1: The slice is just a single Index (a number)
    [(number? slc) 
     (list-ref arr (pos-index slc (length arr)))]
    
    ; Case 2: The slice is our compound struct (start, stop, step)
    [(slice? slc)
     (let* ([start-idx (pos-index (slice-start slc) (length arr))]
            [stop-idx (pos-index (slice-stop slc) (length arr))]
            [step-val (slice-step slc)]
            
            ; "drop" removes elements from the beginning up to the start index
            [chopped-front (drop arr start-idx)]
            
            ; "take" keeps elements up to the stop index. 
            ; We calculate how many to keep by subtracting start from stop.
            [num-to-keep (max 0 (- stop-idx start-idx))]
            [target-segment (take chopped-front num-to-keep)])
       
       ; Finally, we pass that segment into the filter/step function we just built
       (filter/step target-segment step-val))]))

; mk-Slice/testing : [#:start Index] [#:stop Index] [#:step Number] -> Slice
; API function for the autograder to construct a Slice with default values.
(define (mk-Slice/testing #:start [start 0] #:stop [stop -1] #:step [step 1])
  (slice start stop step))
