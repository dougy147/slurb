(defun is-prime (x)
  (if (< x 2) nil)
  (loop for i from 2 to (isqrt x) do
	(when (= (mod x i) 0) (setq x nil) (return)))
  x)

(defun generate-primes (n)
  (if (= n 0) '())
  (if (= n 1) '(2))
  (setq primes '(2))
  (setq v 3) ; potential prime
  (loop while (not (= (length primes) n)) do
	(when (is-prime v) (nconc primes (list v)))
	(incf v 2))
  primes)

(defun get-prime (index primes-list)
  (nth index primes-list))

(defun shuffle-primes (primes-list)
  (setq original-length (length primes-list))
  (setq new-primes-list '())
  (loop while (< (length new-primes-list) original-length) do
	(setq index (random (length primes-list)))
	(setq popped (nth index primes-list))
	(setq primes-list (nconc (subseq primes-list 0 index) (subseq primes-list (+ index 1) (length primes-list))))
	(setq new-primes-list (nconc new-primes-list (list popped))))
  new-primes-list)

