(load "src/spit.lisp")

(defun read-whole-file-as-bytes (filename)
  "Read whole file as a byte array."
  (setq size (with-open-file (file filename)
	       (file-length file)))
  (with-open-file (input-stream filename
				:direction :input
				:element-type '(unsigned-byte 8))
    (let ((buf (make-array size :element-type (stream-element-type input-stream))))
      (loop for pos = (read-sequence buf input-stream)
	    while (plusp pos)
	    collect buf))))

(defun read-whole-file-as-string (filename)
  "Read whole file as a single string."
  (map 'string #'code-char (nth 0 (read-whole-file-as-bytes filename))))

(defun string-to-char-list (content)
  "Convert string to a char list."
  (loop while (> (length content) 0) do
	(setq c (subseq content 0 1))
	(setq content (subseq content 1 (length content)))
	collect c))

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

; Dictionnary
(defun build-dictionnary (char-list primes)
  (setq *char-dict* (make-hash-table :test 'equal)) ; important the test equal!!
  (setq index 0)
   (defun insert-in-dict (chr index dict)
     (if (gethash chr *char-dict*)
       (progn
	 (setq cur-value (gethash chr *char-dict*))
	 (setq new-value (* cur-value (get-prime index primes)))
	 (setf (gethash chr *char-dict*) new-value))
       (progn
	 (setq init-value (get-prime index primes))
	 (setf (gethash chr *char-dict*) init-value))))
  (defun consume-list (l)
    (if (not (= (length l) 0))
      (progn
	(setq c (subseq l 0 1))
	(insert-in-dict c index *char-dict*)
	(incf index)
	(consume-list (subseq l 1 (length l))))))
  (consume-list char-list)
  *char-dict*)

(defun rebuild-content-as-string-from-dict (dict content primes)
  (setq rebuilt "")
  (loop while (not (= (length rebuilt) (length content))) do
	(loop for i from 0 to (- (length primes) 1) do
	      (loop for k being the hash-keys of dict do
		    (if (= (mod (gethash k dict) (nth i primes)) 0)
		      (setq rebuilt (concatenate 'string rebuilt k))))))
  rebuilt)

; USAGE
(defun usage (exit-code)
  (format t "Usage: ./slurb <input_file|input_string>")
  (exit))

(defun consume-content ()
  (if (not (nth 1 *posix-argv*)) ; for sbcl
    (progn
      (format t "ERROR: No input provided.~%")
      (usage 1))
    (progn
      (let (( input (nth 1 *posix-argv* ))) ; for sbcl
	(let (( content
		(if (not (probe-file input))
		  ; Input is not a file
		  (progn
		    ;(read-input-string input)
		    input)
		  ; Input is a file
		  (progn
		    ;(nth 0 (read-whole-file-as-bytes input)))
		    (read-whole-file-as-string input))
		  )))
	  content)))))

; MAIN
(defun main ()
  ; Consume the full input
  (setq content (consume-content))

  ; Build primes and dictionnary
  (setq primes (generate-primes (length content)))
  ;(print primes)
  (setq dict (build-dictionnary content primes))
  ;(loop for k being the hash-key of dict do (print k))

  ; Obfuscate in various languages (implemented "src/spit.lisp")
  ; Spit: script that rebuilds the original command/script
  ; Execute: language in which the original command/script is meant to be evaluated TODO
  (setq python      (spit-in-python      content primes dict :execute "python"))
  (setq common-lisp (spit-in-common-lisp content primes dict :execute "common-lisp"))
  (setq bash        (spit-in-bash        content primes dict :execute "bash"))
  (setq perl        (spit-in-perl        content primes dict :execute "perl"))
  )
