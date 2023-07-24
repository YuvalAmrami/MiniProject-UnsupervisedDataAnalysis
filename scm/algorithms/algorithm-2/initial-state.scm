(load "libs/macros.scm")
(load "libs/lib.scm")
#;#;
(define (make-Ais-BCoords-v2 Ac Bcoordc Bc ABss)
  ())

(define (BCoordc-Bc->-zero-BCoords BCoordc Bc)
  (map (lambda (f) (f))
       (make-list BCoordc
		  (lambda ()
		    (make-vector Bc 0)))))

(define (Ais-ABss->!zero-BCoordc->BCoordc! BCoordc Ais ABss)
  (lambda (Ais ABss)
    (if (not (null? Ais))
	(with* ([Ais (Ai . Ais)]
		[ABss (ABs . ABss)])
	       (for-each (lambda (AB)
			   (vector-fset! (list-ref BCoords Ai)
					 AB
					 (lambda (x) (+ x 1))))
			 ABs)
	       (update-BCoords! Ais ABss)))))

(define (make-Ais-BCoords Ac BCoordc Bc ABss)
  (let ([BCoords (map (lambda (f) (f))
		      (make-list BCoordc
				 (lambda ()
				   (make-vector Bc 0))))])
    (letrec ([update-BCoords! (lambda (Ais ABss)
				(if (not (null? Ais))
				    (with* ([Ais (Ai . Ais)]
					    [ABss (ABs . ABss)])
					   (for-each (lambda (AB)
						       (vector-fset! (list-ref BCoords Ai)
								     AB
								     (lambda (x) (+ x 1))))
						     ABs)
					   (update-BCoords! Ais ABss))))]
	     [generate-random-Ais
	      ;;; could cause error because there is no guaranty that every i is assigned to An A
	      (lambda (from)
		(if (< from Ac)
		    (cons (random BCoordc)
			  (generate-random-Ais (+ from 1)))
		    '()))])
      (let ([Ais (generate-random-Ais 0)])
	(update-BCoords! Ais ABss)
	(list Ais BCoords)))))
