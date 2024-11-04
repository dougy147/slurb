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

