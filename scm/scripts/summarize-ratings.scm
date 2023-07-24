(load "libs/macros.scm")
(load "libs/io.scm")
(load "libs/lib.scm")
(load "libs/calc-probs.scm")

(define u.m-><u.m-cont>-<maps-u-m>
  (lambda (u.m)
    (list (u.m->u.m-continuous u.m)
	  (list->vector (u.m->us u.m))
	  (list->vector (u.m->ms u.m)))))


(with (io-read "u.m") u.m
      (with (u.m-><u.m-cont>-<maps-u-m> u.m) (u.m-cont us ms)
	    (io-write "u.m-cont" u.m-cont)
	    (io-write "us" us)
	    (io-write "ms" ms)
	    (with (u.m->vecs-<m-p>-<m-m-p> u.m-cont) (vec-m-p vec-m-m-p)
		  (io-write "vec-m-p" vec-m-p)
		  (io-write "vec-m-m-p" vec-m-m-p))))
