(defun build-var-names-dict ()
  (setf *random-state* (make-random-state t))
  (setq *var-names-dict* (make-hash-table))
  (setq char-dict "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

  (defun random-var-name (size)
    (setq var-name "")
    (loop while (< (length var-name) size) do
	  (setq current-char (elt char-dict (random (length char-dict))))
	  (setq var-name (concatenate 'string var-name (string current-char))))
    var-name)

  (setf (gethash 'primes *var-names-dict*)  (random-var-name 3))
  (setf (gethash 'primes-shortened *var-names-dict*)  (random-var-name 1))
  (setf (gethash 'dico *var-names-dict*)    (random-var-name 4))
  (setf (gethash 'dico-shortened *var-names-dict*)  (random-var-name 2))
  (setf (gethash 'rebuilt *var-names-dict*) (random-var-name 5))
  (setf (gethash 'index *var-names-dict*)   (random-var-name 6))
  (setf (gethash 'len-primes *var-names-dict*)   (random-var-name 7)) ; for perl
  )

(defun spit-in-python (content primes dict &execute lang)
  (build-var-names-dict)
  (with-open-file (slurbed "./slurbed.py"
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
    ;(format slurbed "#!/usr/bin/env python3~%")

    (format slurbed "~a=[" (gethash 'primes *var-names-dict*))
    (loop for i from 0 to (- (length primes) 1) do
	  (format slurbed "~d," (nth i primes)))
    (format slurbed "]~%")

    (format slurbed "~a={" (gethash 'dico *var-names-dict*))
    (loop for k being the hash-keys of dict do
	  (progn (setq quotes "\"") (setq c k))
	  (if (string= k #\newline) (progn (setq quotes "\"")   (setq c "\\n")))
	  (if (string= k #\\) (progn (setq quotes "'")   (setq c "\\\\")))
	  (if (string= k #\") (progn (setq quotes "'") (setq c "\"")))
	  (if (string= k #\tab) (progn (setq quotes "\"")   (setq c "\\t")))
	  (format slurbed "~a~a~a: ~d," quotes c quotes (gethash k dict)))
    (format slurbed "}~%")
    (format slurbed "~a=\"\"~%" (gethash 'rebuilt *var-names-dict*))
    (format slurbed "~a=0~%" (gethash 'index *var-names-dict*))
    (format slurbed "while (len(~a) < len(~a)):~%" (gethash 'rebuilt *var-names-dict*) (gethash 'primes *var-names-dict*))
    (format slurbed "    for ~a in ~a:~%" (gethash 'dico-shortened *var-names-dict*) (gethash 'dico *var-names-dict*))
    (format slurbed "        if (~a[~a] % ~a[~a] == 0):~%" (gethash 'dico *var-names-dict*) (gethash 'dico-shortened *var-names-dict*) (gethash 'primes *var-names-dict*) (gethash 'index *var-names-dict*))
    (format slurbed "            ~a+=~a~%" (gethash 'rebuilt *var-names-dict*) (gethash 'dico-shortened *var-names-dict*))
    (format slurbed "    ~a+=1~%" (gethash 'index *var-names-dict*))
    (if (or (not lang) (string= lang "python")) (format slurbed "exec(~a)" (gethash 'rebuilt *var-names-dict*)))
    ))

(defun spit-in-common-lisp (content primes dict &execute lang)
  (build-var-names-dict)
  (with-open-file (slurbed "./slurbed.lisp"
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
    (format slurbed "(setq ~a '(" (gethash 'primes *var-names-dict*))
    (loop for i from 0 to (- (length primes) 1) do
	  (format slurbed "~d " (nth i primes)))
    (format slurbed "))")

    (format slurbed "(setq ~a (make-hash-table :test 'equal))" (gethash 'dico *var-names-dict*))
    (loop for k being the hash-keys of dict do
	  (setq c k)
	  ;(if (string= k #\newline) (progn (setq quotes "\"")   (setq c "\\n")))
	  (if (string= k #\\) (setq c "\\\\"))
	  (if (string= k #\") (setq c "\\\""))
	  ;(if (string= k #\tab) (progn (setq quotes "\"")   (setq c "\\t")))
	  (format slurbed "(setf (gethash \"~a\" ~a) ~d)" c (gethash 'dico *var-names-dict*) (gethash k dict))
	  )
    (format slurbed "(setq ~a \"\")" (gethash 'rebuilt *var-names-dict*))
    (format slurbed "(setq ~a 0)" (gethash 'index *var-names-dict*))
    (format slurbed "(loop while (< (length ~a) (length ~a)) do" (gethash 'rebuilt *var-names-dict*) (gethash 'primes *var-names-dict*))
    (format slurbed "(loop for ~a being the hash-keys of ~a do" (gethash 'dico-shortened *var-names-dict*) (gethash 'dico *var-names-dict*))
    (format slurbed "(if (= (mod (gethash ~a ~a) (nth ~a ~a)) 0)" (gethash 'dico-shortened *var-names-dict*) (gethash 'dico *var-names-dict*) (gethash 'index *var-names-dict*) (gethash 'primes *var-names-dict*))
    (format slurbed "(setf ~a (concatenate 'string ~a ~a))))" (gethash 'rebuilt *var-names-dict*) (gethash 'rebuilt *var-names-dict*) (gethash 'dico-shortened *var-names-dict*))
    (format slurbed "(incf ~a))" (gethash 'index *var-names-dict*))
    (if (or (not lang) (string= lang "common-lisp")) (format slurbed "(eval (read-from-string ~a))" (gethash 'rebuilt *var-names-dict*)))
    (if (string= lang "bash") (format slurbed "(shell ~a)" (gethash 'rebuilt *var-names-dict*)))
    ))

(defun spit-in-bash (content primes dict &execute lang)
  (build-var-names-dict)
  (with-open-file (slurbed "./slurbed.sh"
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
    ;(format slurbed "#!/bin/bash~%")

    ; primes
    (format slurbed "declare -a ~a=( " (gethash 'primes *var-names-dict*))
    (loop for i from 0 to (- (length primes) 1) do
	  (format slurbed "~d " (nth i primes)))
    (format slurbed ");")

    ; dict
    (format slurbed "declare -A ~a=( " (gethash 'dico *var-names-dict*))
    (loop for k being the hash-keys of dict do
	  (progn (setq quotes "\"") (setq c k))
	  (if (string= k #\newline) (progn (setq quotes "\"")   (setq c "\\n")))
	  (if (string= k #\\) (progn (setq quotes "'")   (setq c "\\\\")))
	  (if (string= k #\") (progn (setq quotes "'") (setq c "\"")))
	  (if (string= k #\tab) (progn (setq quotes "\"")   (setq c "\\t")))
	  (format slurbed "[~a~a~a]=~d " quotes c quotes (gethash k dict)))
    (format slurbed ");")

    (format slurbed "for ~a in ${~a[@]}; do " (gethash 'primes-shortened *var-names-dict*) (gethash 'primes *var-names-dict*))
    (format slurbed "for ~a in \"${!~a[@]}\"; do " (gethash 'dico-shortened *var-names-dict*) (gethash 'dico *var-names-dict*))
    (format slurbed "if [[ $(echo \"${~a[\"$~a\"]} % $~a\" | bc ) -eq 0 ]]; then " (gethash 'dico *var-names-dict*) (gethash 'dico-shortened *var-names-dict*) (gethash 'primes-shortened *var-names-dict*))
    (format slurbed "if [[ $~a == '\\n' ]]; then " (gethash 'dico-shortened *var-names-dict*))
    (format slurbed "~a=$'\\n';" (gethash 'dico-shortened *var-names-dict*))
    (format slurbed "fi;")
    (format slurbed "~a+=\"$~a\";" (gethash 'rebuilt *var-names-dict*) (gethash 'dico-shortened *var-names-dict*))
    (format slurbed "break;")
    (format slurbed "fi;")
    (format slurbed "done;")
    (format slurbed "done;")
    ;(format slurbed "echo -e $(echo \"${rebuilt}\" $*)")
    (if (or (not lang) (string= lang "bash")) (format slurbed "eval $(echo \"${~a}\" $*)" (gethash 'rebuilt *var-names-dict*)))
    ))


(defun spit-in-perl (content primes dict &execute lang)
  (build-var-names-dict)
  (with-open-file (slurbed "./slurbed.pl"
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
    ;(format slurbed "#!/usr/bin/env perl~%")
    (format slurbed "use utf8;")

    ; primes
    (format slurbed "@~a=(" (gethash 'primes *var-names-dict*))
    (loop for i from 0 to (- (length primes) 1) do
	  (format slurbed "~d," (nth i primes)))
    (format slurbed ");")

    ; dict
    (format slurbed "%~a=(" (gethash 'dico *var-names-dict*))
    (loop for k being the hash-keys of dict do
	  (progn (setq quotes #\")  (setq c k))
	  (if (string= k #\newline) (progn    (setq c "\\n")))
	  (if (string= k #\\) (progn   (setq c "\\\\")))
	  (if (string= k #\") (progn (setq quotes #\') (setq c "\"")))
	  (if (string= k #\$) (progn (setq quotes #\') ))
	  (if (string= k #\tab) (progn  (setq c "\\t")))
	  (format slurbed "~a~a~a=>~a," quotes c quotes (gethash k dict)))
    (format slurbed ");")
    (format slurbed "$~a=\"\";" (gethash 'rebuilt *var-names-dict*))
    (format slurbed "$~a=@~a;" (gethash 'len-primes *var-names-dict*) (gethash 'primes *var-names-dict*))

    (format slurbed "while (length($~a)<$~a) {" (gethash 'rebuilt *var-names-dict*) (gethash 'len-primes *var-names-dict*))
    (format slurbed "for (@~a) {" (gethash 'primes *var-names-dict*))
    (format slurbed "my $~a=$_;" (gethash 'primes-shortened *var-names-dict*))
    (format slurbed "for (keys %~a) {" (gethash 'dico *var-names-dict*))
    (format slurbed "my $~a=$_;" (gethash 'dico-shortened *var-names-dict*))
    (format slurbed "if ($~a{$~a}%$~a==0) {" (gethash 'dico *var-names-dict*) (gethash 'dico-shortened *var-names-dict*) (gethash 'primes-shortened *var-names-dict*))
    (format slurbed "$~a=$~a.$~a;" (gethash 'rebuilt *var-names-dict*) (gethash 'rebuilt *var-names-dict*) (gethash 'dico-shortened *var-names-dict*))
    (format slurbed "break;")
    (format slurbed "};")
    (format slurbed "};")
    (format slurbed "};")
    (format slurbed "};")
    (if (or (not lang) (string= lang "perl")) (format slurbed "eval \"$~a\";" (gethash 'rebuilt *var-names-dict*)))
    (if (string= lang "perl") (format slurbed "`perl -e \"$~a\";`" (gethash 'rebuilt *var-names-dict*)))
    ))
