(load "src/spit.lisp")
(load "src/input.lisp")
(load "src/primes.lisp")

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
  (setq primes (shuffle-primes primes))
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
