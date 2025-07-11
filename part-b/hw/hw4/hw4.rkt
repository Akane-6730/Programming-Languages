#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; p1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; p2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; p3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

;; p4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

;; p5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= 0 (modulo x 5))
                                    (- x)
                                    x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; p6
(define dan-then-dog
  (letrec ([dog-then-dan (lambda () (cons "dog.jpg" dan-then-dog))])
    (lambda () (cons "dan.jpg" dog-then-dan))))

;; p7
(define (stream-add-zero s)
  (lambda () (let ([pr (s)])
               (cons (cons 0 (car pr))
                     (stream-add-zero (cdr pr))))))

;; p8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;; p9
(define (vector-assoc v vec)
  (let ([len (vector-length vec)])
    (letrec ([f (lambda (x)
                  (cond [(= x len) #f]
                        [#t (let ([xth-vec (vector-ref vec x)])
                              (if (and (pair? xth-vec) (equal? v (car xth-vec)))
                                  xth-vec
                                  (f (+ x 1))))]))])
      (f 0))))

;; p10
(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define next-slot 0)
  (define (check-helper v i) (if (>= i n)
                                 #f
                                 (let ([elem (vector-ref cache i)])
                                   (cond [(equal? elem #f) #f]
                                         [(equal? (car elem) v) elem]
                                         [#t (check-helper v (+ i 1))]))))
  (define (check v) (check-helper v 0))
  (lambda (v) (let ([check-result (check v)])
                (if check-result
                    check-result
                    (let ([result (assoc v xs)])
                      (if result (begin (vector-set! cache next-slot result)
                                        (set! next-slot (modulo (+ next-slot 1) n))
                                        result)
                          #f))))))
