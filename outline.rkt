(require (for-syntax
	  racket/base
	  enforest/transformer))

(define-syntax (outline-module-begin stx)
  (syntax-parse stx
		#:datum-literals (top)
		[(_ (top . content))
		 #`(#%module-begin
		    (outline-top . content))]))
