(load "libs/lib.scm")

(define Ais->Ajs (lambda (Ais unduped)
		   (map (lambda (x)
			  (index-of x unduped))
			Ais)))
(define (equaivalance-class-represantive-element-Ais-BCoords Ais BCoords)
  (let* ([unduped (undup Ais)]
	 [Ajs (Ais->Ajs Ais unduped)]
	 [BCoords (map (lambda (i)
			 (list-ref i BCoords))
		       unduped)])
    (list Ajs BCoords)))
(define (equaivalance-class-represantive-element-Ais Ais)
  (Ais->Ajs Ais (undup Ais)))


(define (Ais-BCoords-ABss-ACoords->stepA!-changedA?
	 Ais BCoords ABss ACoords)
  (let [(pAis Ais)]
    (let ([stepA! (lambda ()
		    (set! pAis (map (lambda (x) x) Ais))
		    (step! Ais BCoords ABss ACoords))]
	  [changedA? (lambda ()
		       (ormap (lambda (i j)
				(not (= i j)))
			      Ais pAis))])
      (list stepA! changedA?))))

(define (A.B->Ac-Bc-ABss-BAss A.B)
  (let ([ABss (map cdr (group A.B car = cdr))]
	[BAss (map cdr (full-group A.B cdr = < car <))])
    (list (length ABss)
	  (length BAss)
	  ABss
	  BAss)))

(define (Ais->clustering Ais)
  (let ([clustering-vec (make-vector (+ (apply max Ais) 1) '())])
    (letrec ([loop (lambda (Ais A)
		     (if (null? Ais)
			 clustering-vec
			 (begin
			   (vector-fset! clustering-vec (car Ais)
					 (lambda (x)
					   (cons A x)))
			   (loop (cdr Ais) (+ A 1)))))])
      (map reverse (vector->list (loop Ais 0))))))


(define log-nothing
  (lambda (Ais Bis) '()))
(define announcer
  (lambda (a)
    (lambda (Ais Bis)
      (printf "performing ~a: " a))))
(define cost-log
  (lambda (clustering->cost)
    (lambda (Ais Bis)
      (printf "~a cost : ~a\n"
	      (list Ais Bis)
	      (clustering->cost (Ais->clustering Bis))))))
(define summary-log
  (lambda (get-cached-Ais-Bis clustering->cost)
    (lambda (Ais Bis)
      (let ([Ais0-Bis0 (get-cached-Ais-Bis)])
	(printf "~a -> ~a : ~a -> ~a\n"
		Ais0-Bis0
		(list Ais Bis)
		(with Ais0-Bis0 (_ Bis0)
		      (clustering->cost (Ais->clustering Bis0)))
		(clustering->cost (Ais->clustering Bis)))))))

(define BCoordc-ACoordc-log
  (lambda (Ais Bis)
    (printf "BCoordc ~a\tACoordc ~a\n"
	    (length (Ais->clustering Ais))
	    (length (Ais->clustering Bis)))))
