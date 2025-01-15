(display "Start loading ...")



;---------------------------------excercise-------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;(cond (<p1 > <e 1 >)
;      (<p2 > <e 2 >)
;      (<pn > <e n >))



;(cond (<p1 > <e 1 >)
;      (<p2 > <e 2 >)
;      (else  <e n >))


;(if <predicate> <consequent> <alternative>)
;
;




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

;(return_bigger_two_sum_square 5 6 8)


;--------------------------------square root------------------------------------


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


;--------------------------------new if----------------------------------------

;(define (new-if predicate then-clause else-clause) (
;						    cond (predicate then-clause) 
;						    (else else-clause))
;  )


;--------------------------------cube root-------------------------------------



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





;-----------------------------------------1.9-----------------------------------


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





;---------------------------- recursive version --------------------------------
;(define (expt b n)
;  (if (= n 0)
;    1
;    (* b (expt b (- n 1))))
;  )

;----------------------------iterative version----------------------------------
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b
	       (- counter 1)
	       (* b product)))
  )



;--------------------------fast exponential recursive---------------------------

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))



;---------------------------fast exponential iteration--------------------------

;(define (fast-expt b n)
;    (fast-expt-iter b n 1))
;
;(define (fast-expt-iter b n a)
;    (cond ((= n 0)
;            a)
;          ((even? n)
;            (fast-expt-iter (square b)
;                       (/ n 2)
;                       a))
;          ((odd? n)
;            (fast-expt-iter b
;                       (- n 1)
;                       (* b a)))))
;

;-------------------------multiple excercise recursive--------------------------
;
;(define (multp a b)
;  (if (= b 0)
;    0
;    (+ a (multp a (- b 1)))
;    )
;  )

;-------------------------multiple excercise iterative-------------------------
(define (multp a b)
  (multp-iter a b 0))

(define (multp-iter a counter product)
  (if (= counter 0)
    product
    (multp-iter a (- counter 1) (+ product a)))
  )


;------------------------fast multiple excercise recursive----------------------
(define (double x)
  (+ x x)
  )

(define (halve x)
  (/ x 2)
  )

;(define (fast-multp a b) 
;  (cond ((= b 0) 0)
;	((even? b) (fast-multp (double a) (halve b)))
;	(else (+ a (fast-multp a (- b 1)))))
;  )
    
;------------------------fast multiple excercise iterative---------------------

(define (fast-multp a b)
  (fast-multp-iter a b 0))

(define (fast-multp-iter a counter product)
  (cond ((= counter 0) product)
	((even? counter) (fast-multp-iter (double a) (halve counter) product))
	(else (fast-multp-iter a (- counter 1) (+ a product))))
  )
     


;----------------------------- fib recursive ----------------------------------

(define (fib n) (
		 cond ((= n 0) 0) 
		 ((= n 1) 1)
		 (else (+ 
			 (fib (- n 1)) 
			 (fib (- n 2))))
		 )
  )


;------------------------------ fib iterative ---------------------------------
;(define (fib n)
;  (fib-iter 1 0 n))
;(define (fib-iter a b count)
;  (if (= count 0)
;    b
;    (fib-iter (+ a b) a (- count 1))))
;


;------------------------------- fast fib excercise 1.19 ---------------------
(define (fast-fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q)) ; compute p
		   (+ (* 2 p q) (square q)) ; compute q
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1))))
	)




;----------------------------- prime fermat test -----------------------------
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder
	   (square (expmod base (/ exp 2) m))
	   m))
	(else
	  (remainder
	    (* base (expmod base (- exp 1) m))
	    m))))


;(define (expmod-fast base exp m)
;  (remainder (fast-expt base exp) m))




(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
;  (try-it 3))

;---------------------------- why this is not ok? ---------------------------
;(define (fermat-test n)
;  (define (try-it a)
;    (= (expmod a n n) a))
;  (try-it 2))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))



;---------------------------- excercise 1.27 ----------------------------------
;(define (cong? n) 
;  (congcong n n)
;	)
;
;
;(define (congcong count n)
;  (cond ((= count 1) (display "finished"))
;	((test-cong (- count 1) n) (display (- count 1)) (newline) (congcong (- count 1) n))
;	(else (congcong (- count 1) n))
;	)
;	)
;
;
;(define (test-cong a n)
;  (if (= (expmod a n n) a) #t #f))
;
;(define (gcd a b)
;  (if (= b 0)
;    a
;    (gcd b (remainder a b))))
;

;--------------------------- refined prime test with next function -------------
(define (next i) 
  (if (= i 2) 3 (+ i 2))
  )


;----------------------------- prime test --------------------------------------
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))




;------------------------- exercise 1.22 ---------------------------------------
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))
(define (start-prime-test n start-time)
  (if (fermat-test n)
    (report-prime (- (current-milliseconds) start-time)) 
    (display " is not a prime")))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (next-odd n)
  (if (odd? n)
    (+ 2 n)
    (+ 1 n)))


(define (search-for-primes m count)
  (cond ((= count 0) (newline)) 
	((fermat-test m) (timed-prime-test m) (search-for-primes (next-odd m) (- count 1)))
	(else (search-for-primes (next-odd m) count))
	)
  )




;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------- Procedures as argument -----------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b) 
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))



(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b)))
  )


(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))



(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
	      (accumulate combiner null-value term (next a) next b))))

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
    null-value
    (if (filter a)
      (combiner (term a)
		(filtered-accumulate combiner null-value term (next a) next b filter))
      (filtered-accumulate combiner null-value term (next a) next b filter))))


(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))


(define (sum-cubes-acc a b)
  (accumulate-iter + 0 cube a inc b))

(define (sum-cubes-acc-prime a b)
  (filtered-accumulate + 0 cube a inc b prime?))



(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (product-itself-acc-gcd a b)
  (define (gcd? x)
    (if (= 1 (gcd x b))
      #t
      #f))
  (filtered-accumulate * 1 identity a next b gcd?))



(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum-iter cube a inc b))


(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))





(define (factorial n)
  (product-iter identity 1 inc n)
  )

(define (factorial-acc n)
  (accumulate-iter * 1 identity 1 inc n))

(define (pi-prod n)
  (define (pi-prod-next x)
    (+ x 2))
  (if (odd? n)
    (display "n must be even")
    (/ (* 2 (product square 4.0 pi-prod-next n))
       (* (+ n 1) (product square 3.0 pi-prod-next (- n 1))))))



;----------------------- 1.29 -------------------------------------------------

;(define (integral f a b n)
;  (define h (/ (- b a) n))
;  (define (add-two-h x)
;    (+ x h h))
;  (* (/ h 3.0)
;     (+ (f a)
;	(f (+ a (* n h)))
;	(* 4 (sum f (+ a h) add-two-h b))
;	(* 2 (sum f (+ a (* 2 h)) add-two-h b))
;     )
;  )
;)

;(define (integral f a b dx)
;  (define (add-dx x)
;    (+ x dx))
;  (* (sum f (+ a (/ dx 2.0)) add-dx b)
;     dx))
;
;(define (integral f a b dx)
;  (* (sum f
;	  (+ a (/ dx 2.0))
;	  (lambda (x) (+ x dx))
;	  b)
;     dx))



(define (simpson f a b n)
  (define h (/ (- b a) n))

  (define (y k)
    (f (+ a (* k h))))

  (define (factor k)
    (cond ((or (= k 0) (= k n))
	   1)
	  ((odd? k)
	   4)
	  (else
	    2)))

  (define (term k)
    (* (factor k)
       (y k)))

  (define (next k)
    (+ k 1))

  (if (not (even? n))
    (error "n can't be odd")
    (* (/ h 3)
       (sum term (exact->inexact 0) next n)))
  )



;------------------------- 1.3.3 -----------------------------------------------


(define (close-enough? x y) (< (abs (- x y)) 0.001))


(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
	(cond ((positive? test-value)
	       (search f neg-point midpoint))
	      ((negative? test-value)
	       (search f midpoint pos-point))
	      (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	    (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2))
;       tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;	next
;	(try next))))
;  (try first-guess))


;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2))
;       tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (cond ((close-enough? guess next) next)
;	    (else ((display "trying") (display next) (newline) (try next))))))
;  (try first-guess))



(define (fixed-point f first-guess)

  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try guess step)
    (display-info guess step)                       
    (let ((next (f guess)))
      (if (close-enough? next guess)
	(begin                                  
	  (display-info next (+ 1 step))      
	  next)
	(try next (+ 1 step)))))

  (try first-guess 1))

(define (display-info guess step)
  (display "Step: ")
  (display step)
  (display " ")

  (display "Guess: ")
  (display guess)
  (newline))

(define (xroot x)
  (fixed-point (lambda (y) (average y (/ (log x) (log y)))) 2.0)
  )


(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))



;--------------------------- exercise 1.37 ------------------------------------

;-------------------------- iterative -----------------------------------------
;(define (cont-frac N D k)
;
;  (define (iter i result)
;    (if (= i 0)
;      result
;      (iter (- i 1)
;	    (/ (N i)
;	       (+ (D i) result)))))
;
;  (iter (- k 1)
;	(/ (N k) (D k))))
;
;
;;---------------------------- recursive --------------------------------------
;
;(define (cont-frac N D k)
;
;  (define (cf i)
;    (if (= k i)
;      (/ (N k) (D k))
;      (/ (N i)
;	 (+ (D i) (cf (+ i 1))))))
;
;  (cf 1))









;-------------------------------------------------------------------------------
(display "\nLoaded!")



