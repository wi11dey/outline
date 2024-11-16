(define-library (outline read)
  (export read)
  (import
   (scheme base)
   (rename (read primitive-read)))
  (begin
    (define split-indent (string)
      (let count (
		  (type #f)
		  (current 0)
		  (remaining (string->list string)))
	(if (null? remaining)
	    (values current remaining)
	    (let ((char (car remaining)))
	      (if (and
		   type
		   (not (char=? type char)))
		  (error "mixing tabs and spaces"))
	      (if (or
		   (char=? char #\tab)
		   (char=? char #\space))
		  (count (or type char) (+ current 1) (cdr remaining)))))))

    (define (read port)
      (define (write-enclosed characters)
	(write-char #\()
	(for-each write-char remaining)
	(write-char #\)))
      (parameterize ((current-output-port (open-output-string)))
	(let next-line (
			(line-number 1)
			(indent-unit 0)
			(previous-indent 0))
	  (let ((line (read-line port)))
	    (if (not (eof-object? line))
		(let*-values (
			      (column remaining (split-indent line))
			      (indent-unit' (if (> indent-unit 0)
						indent-unit
						column))
			      (indent indent-remainder (floor/ column indent-unit')))
		  (if (> indent-remainder 0)
		      (error (string-append "inconsistent indentation on line "
					    (number->string line-number)
					    ": based on previous input, indentation should be in multiples of "
					    (number->string indent-unit))))
		  (let ((difference (- indent previous-indent)))
		    (write-string (make-string difference (if (> difference 0) #\( #\))))
		    (if (> difference 0)
			))
		  (if (> column 0)
		      (let*-values ((indent indent-remainder (floor/ column indent-unit')))
			
			(write-string (make-string (- indent previous-indent) #\())
			(for-each write-char remaining)))
		  (begin
		    (write-enclosed remaining)
		    (next-line
		     (+ line-number 1)
		     indent-unit
		     column))))))
	(primitive-read (open-input-string (current-output-port)))))))
