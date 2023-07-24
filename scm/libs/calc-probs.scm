(load "libs/lib.scm")

(define fx->fl exact->inexact)

(define us->p1
  (lambda (us u->mc mc uc)
    (fl* 2.0
	 (fl/ 1.0 (fl+ 1.0 (fx->fl uc)))
	 (apply fl+
		(fl/ 1.0 (fx->fl mc))
		(map (lambda (u)
                       (fl/ 1.0 (fx->fl (u->mc u))))
		     us)))))

(define us->p2
  (lambda (us u->mc mc uc)
    (fl* 2.0
	 (fl/ 1.0 (fl+ 1.0 (fx->fl uc)))
	 (apply fl+
		(fl/ 1.0 (fx->fl (fx* mc (fx- mc 1))))
		(map (lambda (u)
                       (fl/ 1.0
			    (fx->fl (fx* (u->mc u)
					 (fx- (u->mc u) 1)))))
		     us)))))

(define u.m->vecs-<m-p>-<m-m-p>
  (lambda (u.m)
    (let* ([u.ms (u.m->u.ms u.m)]
	   [u->ms (a.b->a->b u.ms)]
	   [u.mc (a.bs->a.bc u.ms)]
	   [u->mc (a.b->a->b u.mc)]
	   [uc (length u.ms)]
	   [m.us (a.b->a.bs (a.b->b.a u.m = < <) equal?)]
	   [mc (length m.us)]
	   [m->us (a.b->a->b m.us)]	   
	   [vec-m-p (make-vec-m-<f-us> m.us
				       (lambda (us) (us->p1 us u->mc mc uc)))]
	   [vec-m-m-p (make-vec<mm-<f-us>> mc m->us
					   (lambda (us) (us->p2 us u->mc mc uc)))])
      (list vec-m-p vec-m-m-p))))

(define (vecMP-m^s->vecM^P vec-m-p m^s)
  (list->vector
   (map (vec-m-p->m->p vec-m-p)
	m^s)))

(define (vecMMP-m^s->vecM^M^P vec-m-m-p m^s)
  (let ([mm->p (vec-m-m-p->mm->p vec-m-m-p)]
	[list-list->vec-vec (lambda (xss)
			      (list->vector
			       (map list->vector
				    xss)))]
	[m^ss (filter (lambda (x)
			(< 1 (length x)))
		      (tails m^s))])
    (list-list->vec-vec
     (map (lambda (m^s)
	    (with m^s (m1 . m2s)
		  (map (lambda (m2)
			 (mm->p m1 m2))
		       m2s)))
	  m^ss))))

(define (vec-m-p->m->p vec-m-p)
  (lambda (m)
    (vector-ref vec-m-p m)))
(define (vec-m-m-p->mm->p vec-m-m-p)
  (letrec
      ([mm->p
	(lambda (m1 m2)
	  (if (< m1 m2)
	      (vector-ref (vector-ref vec-m-m-p m1) (- m2 m1 1))
	      (mm->p m2 m1)))])
    mm->p))
