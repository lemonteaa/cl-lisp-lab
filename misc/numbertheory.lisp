;(defun gcd (a b)
;  (do ((x (max a b) y)
;       (y (min a b) (mod x y)))
;      ((= y 0) x)))

(defun even-pow-decomp (n)
  (if (= n 0) '(0 0)
      (do ((m n (/ m 2))
	   (k 0 (+ k 1)))
	  ((oddp m) (list m k)))))

(defun jacobi-sym (a n)
  (labels ((jacobi-int (m n)
	     (let ((mm (first (even-pow-decomp (mod m n))))
		   (mp (second (even-pow-decomp (mod m n))))
		   (nn (first (even-pow-decomp n)))
		   (acc 1))
	       (if (oddp mp)
		   (if (or (= (mod nn 8) 3) (= (mod nn 8) 5))
		       (setf acc (- acc))))
	       (if (or (= mm 1) (= nn 1))
		   acc
		   (* acc
		      (if (or (= (mod mm 4) 1) (= (mod nn 4) 1)) 1 -1)
		      (jacobi-int nn mm))))))
  (if (> (gcd a n) 1) 0
      (jacobi-int a n))))

(defun modexp (b p m)
  (let ((acc 1)
	(r p)
	(term (mod b m)))
    (loop
       (if (= r 0) (return acc))
       (if (oddp r)
	   (setf acc (mod (* acc term) m)))
       (setf r (floor (/ r 2)))
       (setf term (mod (* term term) m)))))

(defun tonelli-shanks (n p)
  (if (not (= (jacobi-sym n p) 1)) (return-from tonelli-shanks nil))
  (flet ((ord-exp2 (x p) ;Find order of special elements: x^(2^i) = 1 (mod p)
	   (do ((cur (mod x p) (mod (* cur cur) p)) (i 0 (+ i 1))) ((= cur 1) i)))
	 (gen-nonres (p) ;Generate a quadratic nonresidue
	   (let ((z 0)) (loop (setf z (+ (random (- p 1)) 1)) (if (= (jacobi-sym z p) -1)
								  (return z))))))
  (let ((q (first (even-pow-decomp (- p 1))))
	(s (second (even-pow-decomp (- p 1)))))
    (if (= s 1) (return-from tonelli-shanks (modexp n (/ (+ p 1) 4) p)))
    (let ((b 1) (i 0))
      ;Loop invariant: r^2 = na (mod p), done when a = 1 (mod p)
      (do ((r (modexp n (/ (+ q 1) 2) p) (mod (* r b) p))
	   (a (modexp n q p) (mod (* a b b) p)) ;Non-maxial order of 2^i as n is quad-res
	   (c (modexp (gen-nonres p) q p) (mod (* b b) p)) ;Complement, maximal 2^M order
	   (M s i))
	  ((= (mod a p) 1) (if (> r (/ (- p 1) 2)) (- p r) r))
	(setf i (ord-exp2 a p))
	;Order reduction: b^2 has order 2^i from c and combining with a
	;halves its order, since (ab^2)^(2^(i-1)) = (-1)*(-1) = 1
	(setf b (modexp c (expt 2 (- M i 1)) p)))))))

(defun sieve-eratosthenes (n)
  (let ((plst (make-array 100 :fill-pointer 0 :adjustable t))
	(sieve (make-array n :initial-element t)))
    (do ((i 2 (+ i 1)))
	((> (* i i) n) nil)
      (if (aref sieve (- i 1))
	  (do ((j (* i i) (+ j i)))
	      ((> j n) nil)
	    (setf (aref sieve (- j 1)) nil))))
    (do ((i 2 (+ i 1)))
	((> i n) plst)
      (if (aref sieve (- i 1))
	  (vector-push-extend i plst)))))

(defun sieve-eratosthenes-segmented (n)
  (let* ((m (floor (sqrt n)))
	 (plst (sieve-eratosthenes m))
	 (sieve (make-array m :initial-element t)))
    ;One block
    (do ((s (+ m 1) (+ s m)))
	((> s n) plst)
      ;Reset sieve
      (do ((i 0 (+ i 1))) ((>= i m) nil)
	(setf (aref sieve i) t))
      ;Sieve with primes
      (do ((i 0 (+ i 1))
	   (p (aref plst 0) (aref plst (+ i 1))))
	  ((> (* p p) (+ s (- m 1))) nil)
	;Scan and mark
	(do ((j (ceiling (/ s p)) (+ j 1)))
	    ((> (* j p) (+ s (- m 1))) nil)
	  (setf (aref sieve (- (* j p) s)) nil)))
      ;Collect new primes
      (do ((i 0 (+ i 1))) ((>= i m) nil)
	(if (and (<= (+ i s) n) (aref sieve i))
	    (vector-push-extend (+ i s) plst))))))
