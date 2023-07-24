

(define (generate-100-movies-randomly)
  (let ([movies (list->vector (get-full-uncont-ms))]
	[movies-count (length (get-full-uncont-ms))])
    (let ([random-movie
	   (lambda ()
	     (vector-ref movies (random movies-count)))])
      (letrec ([loop (lambda (res size)
		       (if (< size 100)
			   (let ([movie (random-movie)])
			     (if (exists (lambda (m)
					   (= m (movie)))
					 res)
				 (loop res size)
				 (loop (cons movie res) (+ size 1))))
			   (sort < res)))])
	(loop '() 0)))))
