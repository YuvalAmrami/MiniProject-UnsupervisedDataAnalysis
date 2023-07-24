(define-syntax for
  (syntax-rules ()
    ((_ i start step while perform)
     (letrec ((loop (lambda (i)
                      (if while
                          (begin 
                            perform 
                            (loop step))))))
       (loop start)))
    ((_ ...) (error "for" "correct form is (for i start step while perform)"))))

(define-syntax with
  (syntax-rules ()
    [(_ x (a) fx ...)
     (apply (lambda (a)
	      fx ...)
	    x)]
    [(_ x (a . b) fx ...)
     (apply (lambda (a . b)
	      fx ...)
	    x)]
    [(_ x a fx ...)
     (let [(a x)]
       fx ...)]))

(define-syntax with*
  (syntax-rules ()
    [(_ ([a x]) . fx)
     (with a x . fx)]
    [(_ ([a x] . b-x) . fx)
     (with a x
	   (with* b-x . fx))]))

(define-syntax destruct
  (syntax-rules ()
    [(_ (x) a fx)
     (let ([y a])
       (destruct
	x (car y)
	fx))]
    [(_ (x . xs) a fx)
     (let ([y a])
       (destruct
	x (car y)
	(destruct xs (cdr y) fx)))]
    [(_ x a fx)
     (let ([x a])
       fx)]))

(define-syntax let*-destruct
  (syntax-rules ()
    [(_ ([(x1 . xs) a] . x-a-s) . fx)
     (apply (lambda (x1 . xs)
	      (let*-destruct x-a-s . fx))
	    a)]
    [(_ ([x a] . x-a-s) . fx)
     (let ([x a])
       (let*-destruct x-a-s . fx))]
    [(_ () . fx)
     (let () . fx)]))
