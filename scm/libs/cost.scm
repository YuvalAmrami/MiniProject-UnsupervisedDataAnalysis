(load "libs/macros.scm")
(load "libs/lib.scm")

(define (cluster-cost pm pmm C)
  (if (= (length C)
	 1)
      (- (log (pm (car C))))
      (/ (apply +
	      (map (lambda (mm)
		     (apply (lambda (x y)
			      (- (log (pmm x y))))
			    mm))
		   (1->2 C)))
	 (- (length C) 1))))


(define (clustering-cost pm pmm C)
  (apply + (map (lambda (C)
		  (let ([cost (cluster-cost pm pmm C)])
		    (if (= cost -inf.0)
			(error 'clustering-cost (format "found infinity at cluster ~a\n" C))
			cost)))
		C)))

(define (A.B->clustering->cost A.B)
  (with (u.m->vecs-<m-p>-<m-m-p> A.B)
	(m-p-vec m-m-p-vec)
	(let ([m->p (vec-m-p->m->p m-p-vec)]
	      [mm->p (vec-m-m-p->mm->p m-m-p-vec)])
	  (lambda (C)
	    (clustering-cost m->p mm->p C)))))
