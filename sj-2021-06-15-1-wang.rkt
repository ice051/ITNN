#lang racket
(require math)

(define (sigmoid x)
  (/ 1 (+ 1 (exp (- x)))))

(define (deriv-sigmoid x)
  (* (sigmoid x) (- 1 (sigmoid x))))

(define (mse y-true y-pred)
  (let ([y-re (for/sum ([i y-true]
                        [j y-pred])
                (sqr (- i j)))])
    (/ y-re (length y-true))))
#;
(define w (vector -0.838 0.893 -0.768 -1.257 -0.096 -0.004))

(define w (vector (random) (random) (random) (random) (random) (random)))
#;
(define b (vector -1.084 -0.366 0.196))

(define b (vector(random) (random) (random)))

(define (feedforward x)
  (let* ([h1 (sigmoid (+ (* (vector-ref w 0) (list-ref x 0))
                         (* (vector-ref w 1) (list-ref x 1))
                         (vector-ref b 0)))]
         [h2 (sigmoid (+ (* (vector-ref w 2) (list-ref x 0))
                         (* (vector-ref w 3) (list-ref x 1))
                         (vector-ref b 1)))])
    (sigmoid (+ (* (vector-ref w 4) h1)
                (* (vector-ref w 5) h2)
                (vector-ref b 2)))))

(define (train data all-y-trues)
  (define learn-rate 0.1)
  (define epochs 1000)
  (for ([epoch (in-range epochs)])
    (for ([x data]
          [y-true all-y-trues])
      (let* ([sum-h1 (+ (* (vector-ref w 0) (list-ref x 0))
                        (* (vector-ref w 1) (list-ref x 1))
                        (vector-ref b 0))]
             [h1 (sigmoid sum-h1)]
             [sum-h2 (+ (* (vector-ref w 2) (list-ref x 0))
                        (* (vector-ref w 3) (list-ref x 1))
                        (vector-ref b 1))]
             [h2 (sigmoid sum-h2)]
             [sum-o1 (+ (* (vector-ref w 4) h1)
                        (* (vector-ref w 5) h2)
                        (vector-ref b 2))]
             [o1 (sigmoid sum-o1)]
             [y-pred o1]
             [d-l-d-ypred (* -2 (- y-true y-pred))]
             [rate (* learn-rate d-l-d-ypred)]
             [d-ypred-d-w5 (* h1 (deriv-sigmoid sum-o1))]
             [d-ypred-d-w6 (* h2 (deriv-sigmoid sum-o1))]
             [d-ypred-d-b3 (deriv-sigmoid sum-o1)]
             [d-ypred-d-h1 (* (vector-ref w 4) (deriv-sigmoid sum-o1))]
             [d-ypred-d-h2 (* (vector-ref w 5) (deriv-sigmoid sum-o1))]
             [d-h1-d-w1 (* (list-ref x 0) (deriv-sigmoid sum-h1))]
             [d-h1-d-w2 (* (list-ref x 1) (deriv-sigmoid sum-h1))]
             [d-h1-d-b1 (deriv-sigmoid sum-h1)]
             [d-h2-d-w3 (* (list-ref x 0) (deriv-sigmoid sum-h2))]
             [d-h2-d-w4 (* (list-ref x 1) (deriv-sigmoid sum-h2))]
             [d-h2-d-b2 (deriv-sigmoid sum-h2)])
        (vector-set! w 0 (- (vector-ref w 0)
                            (* rate d-ypred-d-h1 d-h1-d-w1)))
        (vector-set! w 1 (- (vector-ref w 1)
                            (* rate d-ypred-d-h1 d-h1-d-w2)))      
        (vector-set! b 0 (- (vector-ref b 0)
                            (* rate d-ypred-d-h1 d-h1-d-b1)))
        (vector-set! w 2 (- (vector-ref w 2)
                            (* rate d-ypred-d-h2 d-h2-d-w3)))
        (vector-set! w 3 (- (vector-ref w 3)
                            (* rate d-ypred-d-h2 d-h2-d-w4)))      
        (vector-set! b 1 (- (vector-ref b 1)
                            (* rate d-ypred-d-h2 d-h2-d-b2)))
        (vector-set! w 4 (- (vector-ref w 4)
                            (* rate d-ypred-d-w5)))
        (vector-set! w 5 (- (vector-ref w 5)
                            (* rate d-ypred-d-w6)))       
        (vector-set! b 2 (- (vector-ref b 2)
                            (* rate d-ypred-d-b3)))
        (if (= 0 (remainder epoch 10))
            (let* ([a (for/list ([x data])
                        (feedforward x))]
                   [loss (mse all-y-trues a)])
              (displayln (string-append
                          "Epoch "
                          (number->string epoch)
                          " loss: "
                          (number->string loss))))
            (void))))))
   
(define data  '((-2 -1) (25 6) (17 4) (-15 -6)))

(define all-y-trues '(1 0 0 1))

(train data all-y-trues)

(define emily '(-7 -3))

(define frank '(20 2))

(feedforward emily)

(feedforward frank)