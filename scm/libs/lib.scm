(define (vector-fset! vec i f)
  (vector-set! vec i (f (vector-ref vec i))))

(define enumarate
  (letrec
      ([f-xs (lambda (xs)
	       (if (null? xs)
		   '()
		   (f-xs-i-is (cdr xs) 0 '())))]
       [f-xs-i-is (lambda (xs i is)
		    (if (null? xs)
			(reverse (cons i is))
			(f-xs-i-is (cdr xs) (+ i 1) (cons i is))))])
    f-xs))

(define undup
  (letrec ([undup (lambda (lst res)
		    (if (null? lst)
			(reverse res)
			(undup (cdr lst)
			       (if (exists (lambda (x) (= x (car lst)))
					   res)			       
				   res
				   (cons (car lst)
					 res)))))])
    (lambda (lst)
      (undup lst '()))))
(define index-of
  (letrec ([index-of (lambda (x lst res)
		       (if (= x (car lst))
			   res
			   (index-of x (cdr lst) (+ res 1))))])
    (lambda (x lst)
      (index-of x lst 0))))

(define remove-consecutive
  (letrec ((aux (lambda (predicate lst res)
		  (if (or (null? lst)
			  (null? (cdr lst)))
		      (append lst res)
		      (let ((x1 (car lst))
			    (x2 (cadr lst))
			    (xs (cddr lst)))
			(if (predicate x1 x2)
			    (aux predicate (cons x2 xs) res)
			    (aux predicate (cons x2 xs) (cons x1 res))))))))
    (lambda (predicate lst)
      (reverse (aux predicate lst '())))))

(define remove-duplicates
  (lambda (lst = <)
    (remove-consecutive = (list-sort < lst))))

(define group
  (lambda (lst key eq grouped)
    (letrec ((xslst
	      (lambda (xs)
		(if (eq? xs '())
		    '()
		    (xslst-gcar-gcdr-glst (cdr xs) (car xs) '() '()))))
	     (xslst-gcar-gcdr-glst
	      (lambda (xs g-car g-cdr g-lst)
		(if (eq? xs '())
		    (cons (cons (key g-car)
				(cons (grouped g-car)
				      (reverse g-cdr)))
			  g-lst)
		    (xscar-xscdr-gcar-gcdr-glst (car xs)
						(cdr xs)
						g-car
						g-cdr
						g-lst))))
	     (xscar-xscdr-gcar-gcdr-glst
	      (lambda (xs-car xs-cdr g-car g-cdr g-lst)
		(if (eq (key g-car)
			(key xs-car))
		    (xslst-gcar-gcdr-glst xs-cdr
					  g-car
					  (cons (grouped xs-car)
						g-cdr)
					  g-lst)
		    (xslst-gcar-gcdr-glst xs-cdr
					  xs-car
					  '()
					  (cons (cons (key g-car)
						      (cons (grouped g-car)
							    (reverse g-cdr)))
						g-lst))))))
      (reverse (xslst lst)))))

(define full-group
  (lambda (lst key key-eq? key< grouped grouped<)
    (group (sort (lambda (a b)
		   (let ((key-a (key a))
			 (key-b (key b)))
		     (or (key< key-a key-b)
			 (and (key-eq? key-a key-b)
			      (grouped< (grouped a)
					(grouped b))))))
		 lst)
	   key key-eq? grouped)))

(define lex<
  (lambda (w1 w2)
    (or (and (null? w1) (not (null? w2)))
	(and (not (null? w1)) (not (null? w2))
             (or (< (car w1) (car w2))
		 (and (= (car w1) (car w2))
		      (lex< (cdr w1) (cdr w2))))))))

(define (tails lst)
  (if (null? lst)
      '()
      (cons lst
	    (tails (cdr lst)))))

(define 1->2
  (lambda (lst)
    (if (null? lst)
	'()
	(append
	 (map (lambda (x)
		`(,(car lst) ,x))
	      (cdr lst))
	 (1->2 (cdr lst))))))

(define interleave
  (letrec ([x-ys (lambda (x ys)
		   (if (null? ys)
		       '()
		       (x-y-ys x (car ys) (cdr ys) '())))]
	   [x-y-ys (lambda (x y ys res)
		     (if (null? ys)
			 (reverse (cons y res))
			 (x-y-ys x (car ys) (cdr ys) `(,x ,y ,@res))))])
    x-ys))


;;; for things a and b, a.b is a list of elements where for each car
;;; is a and cdr is b.
;;;
;;; the . notation is right assosiative
;;;
;;; for a thing a, as is a list of a's, ac represants an amount of a
;;; elements as netural, aa..a is a list of a whose size is the amount
;;; of repeating a characters in the expression
;;;
;;; u is a user represanted by netural
;;;
;;; m is a movie represanted by netural
;;;
;;; p is a probability represanted by a fraction

(define lt-a-lt-b->lt-a-b
  (lambda (a= a< b<)
    (lambda (a-b-1 a-b-2)
      (or (a< (car a-b-1)
	      (car a-b-2))
	  (and (a= (car a-b-1)
		   (car a-b-2))
	       (b< (cdr a-b-1)
		   (cdr a-b-2)))))))

(define a-b->b-a
  (lambda (a-b)
    (cons (cdr a-b)
	  (car a-b))))

(define a.b->b.a
  (lambda (a.b b= b< a<)
    (list-sort (lt-a-lt-b->lt-a-b b= b< a<)
	       (map a-b->b-a a.b))))

(define u.m->u.m-continuous
  (letrec ((aux (lambda (a.b lead)
		  (cond ((null? a.b)
			 '())
			((null? (cdr a.b))
			 `((,lead . ,(cdr (car a.b)))))
			(else
			 `((,lead . ,(cdr (car a.b)))
			   . ,(aux (cdr a.b)
				   (if (= (car (car a.b))
					  (car (cadr a.b)))
				       lead
				       (+ lead 1)))))))))
    (lambda (u.m)
      (aux (a.b->b.a (aux (a.b->b.a u.m = < <)
			  0)
		     = < <)
	   0))))

(define u.m->us
  (lambda (u.m)
    (remove-duplicates (map car u.m)
		       = <)))

(define u.m->ms
  (lambda (u.m)
    (remove-duplicates (map cdr u.m)
		       = <)))

(define u.m->m.us
  (lambda (u.m)
    (full-group u.m cdr = < car <)))

(define u.m->u.ms
  (lambda (u.m)
    (full-group u.m car = < cdr <)))

(define a.b->a->b
  (lambda (lst)
    (let ((vec (list->vector lst)))
      (lambda (i)
	(vector-ref vec i)))))

(define a.bs->a.bc
  (lambda (a.bs)
    (map (lambda (a-bs)
	   (length (cdr a-bs)))
	 a.bs)))

(define range
  (lambda (from to)
    (if (< from to)
	(cons from (range (+ from 1)
			  to))
	'())))


(define indexed-fill
  (letrec ((1D-fill (letrec ((!null-fill (lambda (lead count car-val cdr-lst)
					   `(,car-val
					     . ,(1D-fill (+ lead 1)
							 count
							 cdr-lst))))
			     (1D-fill (lambda (lead count lst)
					(if (null? lst)
					    (map (lambda (a)
						   `(,a . 0))
						 (range lead count))
					    (if (= (car (car lst))
						   lead)
						(!null-fill lead count (car lst) (cdr lst))
						(!null-fill lead count `(,lead . 0) lst))))))
		      1D-fill))
	   (2D-fill (letrec ((2D-fill (lambda (lead count lst)
					(if (null? lst)
					    (map (lambda (lead)
						   `(,lead
						     . ,(1D-fill (+ lead 1) count '())))
						 (range lead (- count 1)))
					    (if (= (car (car lst))
						   lead)
						(!null-fill lead count (cdr (car lst)) (cdr lst))
						(!null-fill lead count '() lst)))))
			     (!null-fill (lambda (lead count car-lst cdr-lst)
					   `((,lead . ,(1D-fill (+ lead 1)
								count
								car-lst))
					     . ,(2D-fill (+ lead 1)
							 count
							 cdr-lst)))))
		      2D-fill)))
    (lambda (lst count)
      (2D-fill 0 count lst))))

(define unkey
  (lambda (lst)
    (map cdr lst)))

(define a-bs->b-a
  (lambda (a-bs)
    (apply append
	   (map (lambda (a)
		  (map (lambda (b)
			 `(,b . ,(car a)))
		       (cdr a)))
		a-bs))))

(define a.bs->a.bbs
  (lambda (a.bs)
    (map (lambda (a-bs)
	   (cons (car a-bs)
		 (1->2 (cdr a-bs))))
	 a.bs)))

(define a.bs->a.b
  (lambda (a.bs)
    (apply append
	   (map (lambda (a-bs)
		  (map (lambda (b)
			 (cons (car a-bs) b))
		       (cdr a-bs)))
		a.bs))))

(define a.b->a.bs
  (lambda (a.b a=)
    (group a.b car a= cdr)))

(define sorted-mutual-binary
  (lambda (1s 2s = <)
    (letrec ([1-1s-2-2s
	      (lambda (a1 1s a2 2s res)
		(cond ((= a1 a2) (1s-2s 1s 2s (cons a1 res)))
		      ((< a1 a2) (1s-2-2s 1s a2 2s res))
		      (else      (1-1s-2s a1 1s 2s res))))]
	     [1s-2s
	      (lambda (1s 2s res)
		(if (or (null? 1s)
			(null? 2s))
		    (reverse res)
		    (1-1s-2-2s (car 1s) (cdr 1s) (car 2s) (cdr 2s) res)))]
	     [1-1s-2s
	      (lambda (a1 1s 2s res)
		(if (null? 2s)
		    (reverse res)
		    (1-1s-2-2s a1 1s (car 2s) (cdr 2s) res)))]
	     [1s-2-2s
	      (lambda (1s a2 2s res)
		(if (null? 1s)
		    (reverse res)
		    (1-1s-2-2s (car 1s) (cdr 1s) a2 2s res)))])
     (1s-2s 1s 2s '()))))


(define make-vec-m-<f-us>
  (lambda (m.us f)
    (list->vector
     (map (lambda (m-us)
	    (f (cdr m-us)))
	  m.us))))

(define make-vec<mm-<f-us>>
  (lambda (mc m->us f)
    (let ((vec (make-vector (- mc 1))))
      (for i 0 (+ i 1) (< i (- mc 1))
	   (vector-set!
	    vec i
 	    (let* ((vec-i-length (- mc i 1))
		   (vec-i (make-vector vec-i-length)))
	      (for j 0 (+ j 1) (< j vec-i-length)
		   (vector-set!
		    vec-i j
		    (f (sorted-mutual-binary (m->us i)
					     (m->us (+ i j 1))
					     = <))))
	      vec-i)))
      vec)))
