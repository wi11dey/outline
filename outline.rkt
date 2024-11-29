#lang typed/racket/base
(require (for-syntax typed/racket/base
                     syntax/parse
                     enforest/operator)
         racket/port
         typed/racket/unsafe
         enforest)
(require/typed shrubbery/parse
               [parse-all
                (->*
                 (Input-Port)
                 (#:source Any
                  #:mode (U 'top 'interactive 'line 'text)
                  #:start-column Exact-Nonnegative-Integer)
                 (Syntaxof Any))])
(unsafe-provide (rename-out [outline-read-syntax read-syntax]
                            [outline-read read]))

(define-syntax outline-=
  (infix-operator (list)
                  'macro
                  (lambda (type in-space)
                    (values type in-space))
                  'left))

(define-syntax outline-of
  (infix-operator (list)
                  'macro
                  (lambda (type in-space)
                    (values type in-space))
                  'left))

(define-syntax outline-for
  (infix-operator (list)
                  'macro
                  (lambda (type in-space)
                    (values type in-space))
                  'left))

(define-syntax outline-from
  (infix-operator (list)
                  'macro
                  (lambda (type in-space)
                    (values type in-space))
                  'left))

(define-syntax (module-begin stx)
  (syntax-parse stx
    #:datum-literals (multi)
    [(_ (multi (group n1:number (op o) n2:number)))
     #'(#%module-begin (o 'n1 'n2))]))

(define translation-dictionary : (Immutable-HashTable Char Char)
  #hasheqv([#\\ . #\~]
           [#\| . #\¦]))

(define (translate-bytes! [bytes : Bytes]) : Void
  (for ([i (in-naturals)]
        [byte bytes])
    (let ([replacement (hash-ref translation-dictionary (integer->char byte) #f)])
      (when replacement
        (bytes-set! bytes i (char->integer replacement))))))

(define (translate [in : Input-Port]) : Input-Port
  (filter-read-input-port
   in
   (λ ([bytes : Bytes] result)
     (translate-bytes! bytes)
     result)
   (λ ([bytes : Bytes] _skip _evt result)
     (translate-bytes! bytes)
     result)
   #t))

(define (outline-read-syntax src [in : Input-Port]) : (Syntaxof Any)
  (parse-all (translate in) #:source src))

(define (outline-read [in : Input-Port]) : Any
  (syntax->datum (outline-read-syntax #f in)))
