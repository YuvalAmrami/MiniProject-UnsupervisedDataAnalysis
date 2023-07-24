(load "libs/caching.scm")
(load "libs/data-set.scm")
(load "libs/lib.scm")
(load "libs/processes.scm")
(load "libs/cost.scm")
(load "libs/calc-probs.scm")
(load "algorithms/algorithm-2.scm")
(load "algorithms/algorithm-2/lib.scm")
(load "algorithms/algorithm-2/initial-state.scm")
(load "algorithms/algorithm-1.scm")

(define (name-compute->store-based-cached-getter name compute)
  (let ([get (store-name-compute->store-based-cache-getter
	      (string-append "from-data-set-" name)
	      compute)])
    get))

(define get-full-uncont-unfiltered-u.m
  (name-compute->store-based-cached-getter
   "full-uncont-unfiltered-u.m"
   (lambda ()
     (raw-ratings-file->u.m "../ml-1m/ratings.dat"))))

(define get-full-uncont-unfiltered-m.us
  (cache-storage-map "full-uncont-unfiltered-m.us"
		     u.m->m.us get-full-uncont-unfiltered-u.m))

(define get-full-uncont-u.m
  (name-compute->store-based-cached-getter
   "full-uncont-u.m"
   (lambda ()
     (filter-u.m (get-full-uncont-unfiltered-u.m) 20 10))))

(define get-fullUncontUnfilteredM-fullUncontUc-s
  (name-compute->store-based-cached-getter
   "fullUncontUnfilteredM-fullUncontUc-s"
   (lambda ()
     (map (lambda (m-us)
	    (destruct (m . us) m-us
		      (list m
			    (length (sorted-mutual-binary
				     us (get-full-uncont-us)
				     = <)))))
	  (get-full-uncont-unfiltered-m.us)))))

(define get-full-uncont-ms
  (cache-storage-map "full-uncont-ms"
		     u.m->ms get-full-uncont-u.m))
(define get-full-uncont-us
  (cache-storage-map "full-uncont-us"
		     u.m->us get-full-uncont-u.m))

(define get-full-u.m
  (cache-storage-map "full-u.m"
		     u.m->u.m-continuous get-full-uncont-u.m))

(define get-full-vecMP-vecMMP
  (cache-storage-map "full-mp-mmp"
		     u.m->vecs-<m-p>-<m-m-p> get-full-u.m))

(define get-full-uncont-mts
  (name-compute->store-based-cached-getter
   "full-mts"
   (lambda ()
     (raw-movies-file->movie-title-list "../ml-1m/movies.dat"))))

(define (read-movies file)
  (with-input-from-file file
    (letrec ([m-ms (lambda (m ms)
		     (if (eof-object? m)
			 (reverse ms)
			 (m-ms (read) (cons m ms))))])
      (lambda ()
	(m-ms (read) '())))))

(define verify-movies-are-numbers
  (let ([error-invalid-number
	 (lambda (movie)
	   (display (format "Movie ~a ignored because it is not a valid number\n" movie)
		    (standard-error-port 'block (current-transcoder))))])
    (letrec ([ms-read-ms (lambda (ms read-ms)
			   (if (null? read-ms)
			       (reverse ms)
			       (ms-read-m-read-ms ms (car read-ms) (cdr read-ms))))]
	     [ms-read-m-read-ms (lambda (ms read-m read-ms)
				  (if (number? read-m)
				      (ms-read-ms (cons read-m ms) read-ms)
				      (begin
					(error-invalid-number read-m)
					(ms-read-ms ms read-ms))))])
      (lambda (movies)
	(ms-read-ms '() movies)))))

(define verify-movies-exist
  (let ([error-not-enought-ratings
	 (lambda (movie count)
	   (display (format "Movie ~a ignored because it has only ~a ratings\n" movie count)
		    (standard-error-port 'block (current-transcoder))))])
    (letrec ([ms-read-ms-MCs (lambda (ms read-ms MCs)
			       (if (null? read-ms)
				   (reverse ms)
				   (ms-read-m-read-ms-MCs ms (car read-ms) (cdr read-ms) MCs)))]
	     [ms-read-m-read-ms-MCs (lambda (ms read-m read-ms MCs)
				      (if (null? MCs)
					  (begin
					    (error-not-enought-ratings read-m 0)
					    (ms-read-ms ms read-ms))
					  (ms-read-m-read-ms-MC-MCs ms read-m read-ms (car MCs) (cdr MCs))))]
	     [ms-read-ms (lambda (ms read-ms)
			   (if (null? read-ms)
			       (reverse ms)
			       (begin
				 (error-not-enought-ratings (car read-ms) 0)
				 (ms-read-ms ms (cdr read-ms)))))]

	     [ms-read-m-read-ms-MC-MCs (lambda (ms read-m read-ms MC MCs)
					 (destruct (M C) MC
						   (cond [(= read-m M)
							  (if (< C 10)
							      (begin
								(error-not-enought-ratings M C)
								(ms-read-ms-MCs ms read-ms MCs))
							      (ms-read-ms-MCs (cons M ms) read-ms MCs))]
							 [(< read-m M)
							  (begin
							    (error-not-enought-ratings read-m 0)
							    (ms-read-ms-MC-MCs ms read-ms MC MCs))]
							 [else
							  (ms-read-m-read-ms-MCs ms read-m read-ms MCs)])))]
	     
	     [ms-read-ms-MC-MCs (lambda (ms read-ms MC MCs)
				  (if (null? read-ms)
				      (reverse ms)
				      (ms-read-m-read-ms-MC-MCs ms (car read-ms) (cdr read-ms) MC MCs)))])
      (lambda (movies)
	(ms-read-ms-MCs '() movies (get-fullUncontUnfilteredM-fullUncontUc-s))))))


(define (print-assignemnt-results clustering->cost m->uncont-m-t clustering)
  (let ([clustering (filter (lambda (x) (> (length x)
					   0))
			    clustering)])
    (for-each (lambda (ms)
		(printf "~a\n"
			(apply string-append
			       (interleave ", "
					   (map (lambda (m)
						  (with (m->uncont-m-t m)
							(m t)
							(format "~a ~s" m t)))
						ms)))))
	      clustering)
    (printf "~a\n"
	    (clustering->cost clustering))))

(define run-to-fixed-point-or-timed
  (lambda (stepA! changedA? stepB! changedB? duration)
    (let ([reached-fix-point?-stepA!-stepB!
	   (make-is-fixed-point? stepA! changedA? stepB! changedB?)]
	  [reached-duration?
	   (duration->reached-duration? duration)])
      (with reached-fix-point?-stepA!-stepB!
	    (reached-fix-point? stepA! stepB!)
	    (let ([process! (error 'run-to-fixed-point-or-timed "did not implement process! yet")])
	      (process!-run-while process! (lambda ()
					     (not (or (reached-fix-point?)
						      (reached-duration?))))))))))

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
			     (let*-destruct
			      ([reached-duration? (duration->reached-duration? (make-time 'time-duration 0 10))]
			       [(is-fixed-point? stepA! stepB!) (make-is-fixed-point? stepA! changedA? stepB! changedB?)])
			      (process!-run-while (process!-scheduler
						   (chooser-inded-timed changedA? changedB? (make-time 'time-duration 0 2))
						   (step!->process!
						    (lambda ()
						      (apply-before-after-logging
						       before-A after-A stepA!)))
						   (step!->process!
						    (lambda ()
						      (apply-before-after-logging
						       before-B after-B stepB!))))
						  (lambda ()
						    (and (not (is-fixed-point?))
							 (not (reached-duration?)))))))))
	     (list Ais Bis))))

(define (A.B-BCoordc-ACoordc->logged-run log? A.B clustering->cost BCoordc ACoordc)
  (let ([cost-log (cost-log clustering->cost)])
    (loggers->run-indempotance-switch-to-fix-point
     A.B BCoordc ACoordc
     log-nothing (if log?
		     BCoordc-ACoordc-log
		     log-nothing)
     log-nothing (if log?
		     (lambda (Ais Bis)
		       (printf "cost ~a\n"
			       (clustering->cost (Ais->clustering Bis)))
		       (BCoordc-ACoordc-log Ais Bis))
		     log-nothing)
     (if log?
	 (lambda (Ais Bis)
	   (BCoordc-ACoordc-log Ais Bis))
	 log-nothing)
     log-nothing)))



(with (command-line-arguments)
      (algorithm movies-file . args)
      (letrec ([string-index (lambda (str lst)
			       (if (null? lst)
				   (error 'string-index "list dose not contain string")
				   (if (string=? (car lst) str)
				       0
				       (+ 1 (string-index str (cdr lst))))))])
	(let ([log? (exists (lambda (s) (string=? s "--log")) args)]
	      [BCoordc (if (exists (lambda (s) (string=? s "--BCoordc")) args)
			   (string->number
			    (list-ref args
				      (+ 1 (string-index "--BCoordc" args))))
			   50)]
	      [ACoordc (if (exists (lambda (s) (string=? s "--ACoordc")) args)
			   (string->number
			    (list-ref args
				      (+ 1 (string-index "--ACoordc" args))))
			   35)]
	      [algorithm (string->number algorithm)])
	  (letrec ([get-uncont-ms
		    (compute->cashed-getter
		     (lambda ()
		       (verify-movies-exist (sort < (verify-movies-are-numbers (read-movies movies-file))))))]

		   [get-uncont-mts
		    (compute->cashed-getter
		     (lambda ()
		       (sorted-mutual-binary ((lambda ()
						(get-full-uncont-mts)))
					     ((lambda ()
						(get-uncont-ms)))
					     (lambda (m-t m)
					       (with m-t
						     (m1 _t)
						     (= m1 m)))
					     (lambda (m-t m)
					       (with m-t
						     (m1 _t)
						     (< m1 m))))))]

		   [uncont-m->full-m
		    (lambda (uncont-m)
		      (index-of uncont-m ((lambda ()
					    (get-full-uncont-ms)))))]
		   
		   [m->uncont-m-t
		    (let ([get-uncont-vec-mts (compute->cashed-getter
					       (lambda ()
						 (list->vector ((lambda ()
								  (get-uncont-mts))))))])
		      (lambda (m)
			(vector-ref ((lambda ()
				       (get-uncont-vec-mts)))
				    m)))]

		   [get-vecMP-vecMMP
		    (compute->cashed-getter
		     (lambda ()
		       (let ([full-ms-subset
			      (map uncont-m->full-m ((lambda ()
						       (get-uncont-ms))))])
			 (with ((lambda ()
				  (get-full-vecMP-vecMMP)))
			       (vec-m-p vec-mm-p)
			       (list (vecMP-m^s->vecM^P vec-m-p full-ms-subset)
				     (vecMMP-m^s->vecM^M^P vec-mm-p full-ms-subset))))))]

		   [get-uncont-u.m
		    (compute->cashed-getter
		     (lambda ()
		       (let* ([uncont-u.m
			       (filter (lambda (u-m)
					 (exists (lambda (m)
						   (= m (cdr u-m)))
						 ((lambda ()
						    (get-uncont-ms)))))
				       ((lambda ()
					  (get-full-uncont-u.m))))]
			      [uncont-u.ms
			       (filter (lambda (u-ms)
					 (> (length (cdr u-ms))
					    1))
				       (u.m->u.ms uncont-u.m))]
			      [uncont-us
			       (remove-consecutive = (map car uncont-u.ms))])
			 (filter (lambda (u-m)
				   (exists (lambda (u)
					     (= u (car u-m)))
					   uncont-us))
				 uncont-u.m))))]

		   [get-uncont-us
		    (cache-map u.m->us (lambda ()
					 (get-uncont-u.m)))]

		   [get-u.m
		    (compute->cashed-getter
		     (lambda ()
		       (let ([uncont-u->u
			      (lambda (u)
				(index-of u ((lambda ()
					       (get-uncont-us)))))]
			     [uncont-m->m
			      (lambda (m)
				(index-of m ((lambda ()
					       (get-uncont-ms)))))])
			 (let ([uncont-u-m->u-m
				(lambda (u-m)
				  (cons (uncont-u->u (car u-m))
					(uncont-m->m (cdr u-m))))])
			   (map uncont-u-m->u-m
				((lambda ()
				   (get-uncont-u.m))))))))])

	    (cond ((= algorithm 1)
		   (with (get-vecMP-vecMMP) (m-p-vec m-m-p-vec)
			 (let* ([m->p (vec-m-p->m->p m-p-vec)]
				[mm->p (vec-m-m-p->mm->p m-m-p-vec)]
				[C (solution m->p
					     mm->p
					     (u.m->ms (get-u.m)))])
			   
			   (print-assignemnt-results
			    (lambda (C) (clustering-cost m->p mm->p C))
			    m->uncont-m-t
			    C))))
		  
		  ((= algorithm 2)
		   (apply
		    (lambda (Ais Bis)
		      (print-assignemnt-results
		       (A.B->clustering->cost (get-u.m))
		       m->uncont-m-t
		       (Ais->clustering Bis)))
		    (A.B-BCoordc-ACoordc->logged-run
		     log?
		     (get-u.m)
		     (A.B->clustering->cost (get-u.m))
		     BCoordc ACoordc)))

		  (else (error 'algorithm-choice
			       (format "invalid choice ~a"
				       algorithm))))))))
