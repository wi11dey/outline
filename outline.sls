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
      (let (
	    (buffer (get-output-string)))
	(let next-line (
			(line-number 1)
			(line (read-line port))
			(indent-unit #f)
			(previous-indent 0))
	  (if (not (eof-object? line))
	      (let*-values (
			    (column remaining (split-indent line))
			    (effective-indent-unit (or indent-unit column))
			    (indent (call-with-values (lambda () (floor/ column effective-indent-unit))
				      (lambda (result remainder)
					(if (> remainder 0)
					    (error (string-append "inconsistent indentation on line"
								  (number->string line-number))))
					result))))
		
		)))
	(primitive-read (open-input-string buffer))))))
