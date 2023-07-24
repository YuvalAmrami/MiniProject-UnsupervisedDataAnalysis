(define (collect-correlated m->p mm->p clustering m ms)
  (solve m->p mm->p
         (cons (filter (lambda (m2)
			 (or (= m2 m)
			     (< (* (m->p m2) (m->p m))
				(mm->p m m2))))
		       ms)
	       clustering)
	 (filter (lambda (m2)
		   (not (or (= m2 m)
			    (< (* (m->p m2) (m->p m))
			       (mm->p m m2)))))
		 ms)))

(define (solve m->p mm->p clustering movies)
  (if (null? movies)
      (reverse clustering)
      (collect-correlated m->p mm->p
			  clustering
			  (list-ref movies (random (length movies)))
			  movies)))

(define (solution m->p mm->p movies)
  (solve m->p mm->p '() movies))


