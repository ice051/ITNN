#lang racket
(require math)

(define (sigmoid x)
  (/ 1 (+ 1 (exp (- x)))))

(define (feedforward w b x)
  (let ([w*x+b (+ (array-all-fold (array* w x) +) b)])
    (sigmoid w*x+b)))


;1
;(define w (array #(0 1)))
;
;(define b 4)
;
;(define x (array #(2 3)))
;
;(feedforward w b x)


;2
(define w (array #(0 1)))

(define b 0)

(define x (array #(2 3)))

(define h1 (feedforward w b x))

(define h2 h1)

(feedforward w b (array #(h1 h2)))


;3
(define (mse y-true y-pred)
  (let* ([arr-re (array- y-true y-pred)]
         [size (array-size arr-re)])
    (/ (array-all-sum (array-sqr arr-re)) size)))

(define y-true (array #(1 0 0 1)))

(define y-pred (array #(0 0 0 0)))

(mse y-true y-pred)