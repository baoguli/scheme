(display "Start loading ...")
;---------------------------------excercise-------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
(define (square x) (* x x))

(define (square_sum x y) (+ (square x) (square y)))

(define (return_bigger_two_sum_square x y z) (
					      cond ((and (< x y) (< x z)) (square_sum y z))
					      ((and (< y z) (< y x)) (square_sum x z))
					      (else (square_sum x y))
					      )
  )

(define (
	 a_plus_abs_b a b
	 )
  (
   (
    if (> b 0) + -) a b
   )
  )

(return_bigger_two_sum_square 5 6 8)



;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;--------------------------------square root------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))



(define (improve guess x)
  (average guess (/ x guess)))


(define (average x y)
  (/ (+ x y) 2))



;(define (good-enough? guess x)(
;			       < (abs (- (square guess) x)) 0.001
;			       ))

(define (good-enough? guess x)(
			       < (abs (- (improve guess x) guess)) 0.00001)
  )





(define (sqrt x) (
		  sqrt-iter 1.0 x)
  )


;--------------------------------new if----------------------------------

;(define (new-if predicate then-clause else-clause) (
;						    cond (predicate then-clause) 
;						    (else else-clause))
;  )










;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;--------------------------------cube root--------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------





(define (cbrt-iter guess x) (
			     if (good-enough-cube? guess x)
			     guess
			     (cbrt-iter (improve_cb guess x) x))
  )



(define (improve_cb guess x) (
			      / 
				(+ 
				  (/ 
				    x 
				    (square guess)) 
				  (* 2 guess))
				3)
  )




;(define (good-enough? guess x)(
;			       < (abs (- (square guess) x)) 0.001
;			       ))

(define (good-enough-cube? guess x) (
				< (abs (- (improve_cb guess x) guess)) 0.00001)
  )





(define (cbrt x) (
		  cbrt-iter 1.0 x)
  )






(define (cube x) (
		  * x x x)
  )





;-----------------------------------------1.9------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------



;(define (inc x) (
;		 + 1 x)
;  )
;
;(define (dec x) (
;		 - x 1)
;  )
;


;(define (plus a b) (
;		    if (= a 0) 
;		    b 
;		    (inc (plus (dec a) b))
;		    )
;  )


;(define (A x y) (
;		 cond ((= y 0) 0)
;		 ((= x 0) (* 2 y))
;		 ((= y 1) 2)
;		 (else (A (- x 1)
;			  (A x (- y 1))
;			  )
;		       )
;		 )
;  )
;
;
;
;
;(define (f n) (A 0 n))
;(define (g n) (A 1 n))
;(define (h n) (A 2 n))
;(define (k n) (* 5 n n))



(define (fib n) (
		 cond ((= n 0) 0) 
		 ((= n 1) 1)
		 (else (+ 
			 (fib (- n 1)) 
			 (fib (- n 2))))
		 )
  )








(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))







(display "\nLoaded!")

;test if it works





