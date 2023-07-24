(define (parse-result-success parsed rest)
  `(success ,parsed ,rest))

(define (parse-result-error error)
  `(error ,error))

(define (parse-result-dispatch pr success-handle error-handle)
  (cond ((eq? (car pr) 'success)
	 (apply success-handle (cdr pr)))
	((eq? (car pr) 'error)
	 (apply error-handle (cdr pr)))
	(else
	 (error pr (format "incorrect car ~a of parse result ~a whose car should be either success or error" (car pr) pr)))))

(define (parse-result-map f pr)
  (parse-result-dispatch pr
			 (lambda (x lst)
			   (parse-result-success (f x) lst))
			 parse-result-error))

(define (parse-dispatch p success-handle error-handle)
  (lambda (lst)
    (parse-result-dispatch (p lst)
			   success-handle error-handle)))


(define parse-by
  (let ([error (lambda (got-result expected-elem)
		 (format "got ~a when parsing ~a" got-result expected-elem))])
    (lambda (pred what)
      (lambda (lst)
	(cond ((null? lst)
	       (parse-result-error (error '() what)))
	      ((pred (car lst))
	       (parse-result-success (car lst) (cdr lst)))
	      (else
	       (parse-result-error (error (cons (car lst) 'rest) what))))))))

(define (parse-a a)
  (parse-by (lambda (x) (equal? x a))
	    a))

(define (parse-fail reason)
  (lambda (lst)
    (parse-result-error reason)))

(define (parse-add . ps)
  (if (null? ps)
      (lambda (lst)
	(parse-result-error '()))
      (lambda (lst)
	(parse-result-dispatch
	 ((car ps) lst)
	 parse-result-success
	 (lambda (error)
	   (parse-result-dispatch
	    ((apply parse-add (cdr ps)) lst)
	    parse-result-success
	    (lambda (errors)
	      (parse-result-error (cons error errors)))))))))

(define (parse-map f . ps)
  (letrec ([sequence-parse
	    (lambda (ps)
	      (if (null? ps)
		  (lambda (lst)
		    (parse-result-success '() lst))
		  (parse-dispatch
		   (car ps)
		   (lambda (parsed rest)
		     (parse-result-dispatch
		      ((sequence-parse (cdr ps)) rest)
		      (lambda (parseds rest)
			(parse-result-success (cons parsed parseds)
					      rest))
		      (lambda (error)
			(parse-result-error (cons parsed error)))))
		   (lambda (error)
		     (parse-result-error (list error))))))])
    (parse-dispatch (sequence-parse ps)
		    (lambda (parsed rest)
		      (parse-result-success (apply f parsed) rest))
		    parse-result-error)))

(define (parse-unit x)
  (lambda (lst)
    (parse-result-success x lst)))

(define (parse-bind p act)
  (lambda (lst)
    (parse-result-dispatch (p lst)
			   (lambda (x lst)
			     ((act x) lst))
			   parse-result-error)))

(define (parse-join pp)
  (lambda (lst)
    (pp (lambda (x) x))))


(define (parse-star p)
  (lambda (lst)
    ((parse-add (parse-map cons
			   p (parse-star p))
		(lambda (lst)
		  (parse-result-success '() lst)))
     lst)))

(define (parse-star-with-end p pend)
  (lambda (lst)
    ((parse-add (parse-map cons
			   p (parse-star-with-end p pend))
		(parse-map list pend))
     lst)))

(define (parse-plus p)
  (parse-star-with-end p p))

(define (parse-interleave-ignored p1 p2)
  (lambda (lst)
    ((parse-add
      (parse-star-with-end (parse-map (lambda (x _) x)
				      p1 p2)
			   p1)
      (parse-unit '()))
     lst)))

(define (parse-not-followed-by what1 p1 what2 p2)
  (lambda (lst)
    (parse-result-dispatch
     (p1 lst)
     (lambda (x1 ys)
       (parse-result-dispatch
	(p2 ys)
	(lambda (x2 ys)
	  (parse-result-error (format "form ~a parsed as ~a should not be followed by form ~a parsed as ~a"
				      what1 x1 what2 x2)))
	(lambda (err)
	  (parse-result-success x1 ys))))
     parse-result-error)))

(define (parse-must-end p)
  (parse-dispatch p
		  (lambda (res lst)
		    (if (null? lst)
			(parse-result-success res lst)
			(parse-result-error `("expected to end but remaining" ,lst))))
		  parse-result-error))
