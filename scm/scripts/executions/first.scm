(load "libs/macros.scm")
(load "libs/lib.scm")
(load "libs/processes.scm")
(load "libs/calc-probs.scm")
(load "libs/cost.scm")
(load "algorithms/algorithm-1.scm")
(load "algorithms/algorithm-2.scm")
(load "algorithms/algorithm-2/lib.scm")

(define loggers->run-indempotance-switch-to-fix-point
  (lambda (A.B BCoordc ACoordc before-A after-A before-B after-B before after)
    (with* ([(A.B->Ac-Bc-ABss-BAss A.B)
	     (Ac Bc ABss BAss)]
	    [(make-Ais-BCoords Ac BCoordc Bc ABss)
	     (Ais BCoords)]
	    [(make-Ais-BCoords Bc ACoordc Ac BAss)
	     (Bis ACoords)]
	    [(Ais-BCoords-ABss-ACoords->stepA!-changedA?
	      Ais BCoords ABss ACoords) (stepA! changedA?)]
	    [(Ais-BCoords-ABss-ACoords->stepA!-changedA?
	      Bis ACoords BAss BCoords) (stepB! changedB?)])
	   
	   (let* ([activate-logger (lambda (logger)
				     (apply logger
					    (map equaivalance-class-represantive-element-Ais
						 (list Ais Bis))))]
		  [apply-before-after-logging (lambda (before after action)
						(activate-logger before)
						(action)
						(activate-logger after))])
	     (apply-before-after-logging
	      before after (lambda ()
			     (run-indempotance-switching-to-fixed-point
			      (lambda ()
				(apply-before-after-logging
				 before-A after-A stepA!))
			      changedA?
			      (lambda ()
				(apply-before-after-logging
				 before-B after-B stepB!))
			      changedB?)))
	     (list Ais Bis)))))

(define A.B
  '((0 . 0) (0 . 1) (0 . 2) (0 . 3) (0 . 4) (0 . 5)
    (1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5)
    (2 . 2) (2 . 3) (2 . 4) (2 . 5) (2 . 6)
    (3 . 3) (3 . 4) (3 . 5) (3 . 6) (3 . 7)
    (4 . 4) (4 . 5) (4 . 6) (4 . 7) (4 . 0)
    (5 . 1) (5 . 2) (5 . 3)
    (6 . 4) (6 . 5) (6 . 6)
    (7 . 5) (7 . 6)
    (8 . 1) (8 . 2)
    (9 . 2) (9 . 3)

    (10 . 8) (10 . 9) (10 . 10) (10 . 11) (10 . 12) (10 . 13)
    (11 . 9) (11 . 10) (11 . 11) (11 . 12) (11 . 13) (12 . 10)
    (12 . 11) (12 . 12) (12 . 13) (12 . 14) (13 . 11) (13 . 12)
    (13 . 13) (13 . 14) (13 . 15) (14 . 12) (14 . 13) (14 . 14)
    (14 . 15) (14 . 8) (15 . 9) (15 . 10) (15 . 11) (16 . 12)
    (16 . 13) (16 . 14) (17 . 13) (17 . 14) (18 . 9) (18 . 10)
    (19 . 10) (19 . 11)

    (20 . 16) (20 . 17) (20 . 18) (20 . 19) (20 . 20) (20 . 21)
    (21 . 17) (21 . 18) (21 . 19) (21 . 20) (21 . 21) (22 . 18)
    (22 . 19) (22 . 20) (22 . 21) (22 . 22) (23 . 19) (23 . 20)
    (23 . 21) (23 . 22) (23 . 23) (24 . 20) (24 . 21) (24 . 22)
    (24 . 23) (24 . 16) (25 . 17) (25 . 18) (25 . 19) (26 . 20)
    (26 . 21) (26 . 22) (27 . 21) (27 . 22) (28 . 17) (28 . 18)
    (29 . 18) (29 . 19)
    ))

(define (BCoordc-ACoordc->logged-run BCoordc ACoordc)
  (with* ([(A.B->clustering->cost A.B) clustering->cost]
	  [(cost-log clustering->cost) cost-log])
	 (loggers->run-indempotance-switch-to-fix-point
	  A.B BCoordc ACoordc
	  log-nothing log-nothing
	  log-nothing (lambda (Ais Bis)
			(printf " -> ~a"
				(clustering->cost (Ais->clustering Bis))))
	  (lambda (Ais Bis)
	    (printf "algorithm 2:\n~a\n~a"
		    (Ais->clustering Bis)
		    (clustering->cost (Ais->clustering Bis))))
	  (lambda (Ais Bis)
	    (printf "\n~a\n\n"
		    (Ais->clustering Bis))))))

#;
(define (BCoordc-ACoordc->optimal BCoordc ACoordc run duration-in-seconds)
  (let ([start (current-time 'time-monotonic)])
    (letrec (loop (lambda (best-yet time)
		    (< (- time start) duration-in-seconds)
		    (with (loggers->run A.B BCoordc ACoordc)
			  (Ais Bis)
			  ((list Ais Bis) (clustering->cost (Ais->clustering Bis)))))))))
(process!-run-times (step!->process! (lambda ()
				       (BCoordc-ACoordc->logged-run 10 12)))
		    5)

(process!-run-times
 (step!->process!
  (lambda ()
    (with* ([(u.m->vecs-<m-p>-<m-m-p> A.B)
	     (m-p-vec m-m-p-vec)]
	    [(vec-m-p->m->p m-p-vec)
	     m->p]
	    [(vec-m-m-p->mm->p m-m-p-vec)
	     mm->p]
	    [(solution m->p mm->p (u.m->ms A.B))
	     C])
	   (printf "algorithm 1: ~a : ~a\n"
		   (list-sort lex< C) (clustering-cost m->p mm->p C)))))
 10)


#;
(let ([loop (lambda (BCoordc ACoordc)
	      (if (< ACoordc 10)
		  (cons (BCoordc-ACoordc->logged-run BC)
			(loop (+ BCoordc 1) ACoordc))
		  ))]))
