(load "libs/macros.scm")

(define (step!->process! step!)
  (letrec ([process! (lambda ()
		       (step!)
		       process!)])
    process!))

(define (score-driven-stepping-process! step0! calc-score0 step1! calc-score1)
  (letrec ([score-memoizing-helper (lambda (score0 score1)
				     (lambda ()
				      (if (<= score0 score1)
					  (begin
					    (step0!)
					    (score-memoizing-helper (calc-score0) score1))
					  (begin
					    (step1!)
					    (score-memoizing-helper score0 (calc-score1))))))])
    (score-memoizing-helper (calc-score0) (calc-score1))))

(define (indempotance-switching-process!-with-fold step0! changed0? step1! changed1? folder initial)
  (let ([fold-value initial])
    (let ([process! (lambda (index step!-vec changed?-vec)
		      (letrec ([run (lambda (index)
				      (lambda ()
					((vector-ref step!-vec index))
					(let ([changed ((vector-ref changed?-vec index))])
					  (set! fold-value (folder fold-value index changed))
					  (run (mod (if changed
							index
							(+ index 1))
						    2)))))])
			(run index)))]
	  [fold (lambda ()
		  fold-value)])
      (list (process! 0
		      (vector step0! step1!)
		      (vector changed0? changed1?))
	    fold))))

(define (ind-swch-process!-with-fold step0! changed0? step1! changed1? folder initial)
  (with (step!observes->wrap-with-fold folder initial (list step0! changed0?) (list step1! changed1?))
	(fold step0! step1!)
	(list fold
	      (ind-swch-process! step0! changed0? step1! changed1?))))

(define (ind-swch-process! step0! changed0? step1! changed1?)
  (process!-scheduler (indempotance-switching-chooser changed0? changed1?)
		      step0! step1!))

(define (indempotance-switching-chooser changedA? changedB?)
  (letrec ([choose (lambda () (unchosen-yet))]
	   [unchosen-yet
	    (lambda ()
	      (set! choose (index-keepA?-keepB?->choose 0 changedA? changedB?))
	      0)]
	   [index-keepA?-keepB?->choose
	    (lambda (index keepA? keepB?)
	      (lambda ()
		(if (keepA?)
		    (begin
		      (set! choose
			    (index-keepA?-keepB?->choose
			     index keepA? keepB?))
		      index)
		    (let ([index (- 1 index)])
		      (set! choose
			    (index-keepA?-keepB?->choose
			     index keepB? keepA?))
		      index))))])
    (lambda () (choose))))

(define (interleave-choise-of-order order0 order1)
  (let ([count 0])
    (lambda ()
      (set! count (mod (+ count 1)
		       (+ order0 order1)))
      (if (< count order0)
	  0
	  1))))

;;; indempotance switching and timed (which comes first)
;;; for sequence of steps step_i step_i ...
;;; switching when reached indempotance or a time duration
(define (chooser-inded-timed changed0? changed1? duration)
  (let ([changed?-vec (vector changed0? changed1?)])
    (letrec ([choose (lambda ()
		       ((run-0/1 0 1 (duration->reached-duration? duration))))]
	     [run-0/1
	      (lambda (A B reached-duration?)
		(lambda ()
		  (if (and ((vector-ref changed?-vec A))
			   (not (reached-duration?)))
		      A
		      (begin
			(set! choose
			      (run-0/1 B A
				       (duration->reached-duration? duration)))
			B))))])
      (lambda ()
	(choose)))))

(define (step!observes->wrap-with-fold val-index-observe->val val . st!obs-s)
  (let ([obs?-vec (list->vector (map cdr st!obs-s))])
    (apply wrap-steps-with-folder
	   (lambda (val index)
	     (apply val-index-observe->val
		    val index
		    (map (lambda (get) (get))
			 (vector-ref obs?-vec index))))
	   val
	   (map car st!obs-s))))

(define (wrap-steps-with-folder fold init-value . step!s)
  (let ([fold-value init-value])
    (cons (lambda () fold-value)
	  (map (lambda (index step!)
		 (lambda ()
		   (step!)
		   (set! fold-value (fold fold-value index))))
	       (enumarate step!s)
	       step!s))))

(define (interleaving-process! step0! step1!)
  (lambda ()
    (step0!)
    (interleaving-process! step1! step0!)))



(define (process!-scheduler choose-index . processes!)
  (let ([processes!-vector (list->vector processes!)])
    (letrec ([scheduled-process!
	      (lambda ()
		((vector-ref processes!-vector (choose-index)))
		scheduled-process!)])
      scheduled-process!)))

(define (process!-interleave process0! process1!)
  (lambda ()
    (process!-interleave process1! (process0!))))


(define (process!-leap-while process! while)
  (let ([process! (process!)])
    (if (while)
	(process!-leap-while process! while)
	(lambda ()
	  (process!-leap-while process! while)))))

(define process!-leap-num
  (letrec ([loop (lambda (i num process!)
		      (if (< i num)
			  (loop (+ i 1) num (process!))
			  process!))]
	   [run (lambda (process! num)
		  (lambda ()
		    (run (loop 0 num process!) num)))])
    run))


(define (process!-before-each process! do!)
  (lambda ()
    (do!)
    (process!-before-each (process!) do!)))

(define (process!-after-each process! do!)
  (lambda ()
    (let ([process! (process!)])
      (do!)
      (process!-after-each process! do!))))


(define (process!-run-while process! while)
  (if (while)
      (process!-run-while (process!) while)))

(define (process!-run-forever process!)
  (process!-run-forever (process!)))

(define (process!-run-times process! times)
  ((process!-leap-num process! times)))


(define run-indempotance-switching-to-fixed-point
  (lambda (stepA! changedA? stepB! changedB?)
    (with (ind-swch-process!-with-fold stepA! changedA?
				       stepB! changedB?
				       (lambda (l2 index changed)
					 (with l2 (A-changedA B-changedB)
					       (list (list index changed)
						     A-changedA)))
				       '((_ _) (_ _)))
	  (process! AchA-BchB)
	  (let ([not-reached-fix-point? (lambda ()
					  (with* ([(AchA-BchB) (AchA BchB)]
						  [AchA (A chA)]
						  [BchB (B chB)])
						 (or (eq? B '_)
						     (eq? A B)
						     chA chB)))])
	    (process!-run-while process! not-reached-fix-point?)))))

(define run-ind-swtch-to-fixed-point-or-timed
  (lambda (stepA! changedA? stepB! changedB? duration)
    (with (indempotance-switching-process!-with-fold stepA! changedA?
						     stepB! changedB?
						     (lambda (l2 index changed)
						       (with l2 (A-changedA B-changedB)
							     (list (list index changed)
								   A-changedA)))
						     '((_ _) (_ _)))
	  (process! AchA-BchB)
	  (let ([not-reached-fix-point? (lambda ()
					  (with* ([(AchA-BchB) (AchA BchB)]
						  [AchA (A chA)]
						  [BchB (B chB)])
						 (or (eq? B '_)
						     (eq? A B)
						     chA chB)))]
		[not-reached-time? (let ([start (current-time 'time-monotonic)])
				     (lambda ()
				       (time<? (time-difference (current-time 'time-monotonic) start)
					       duration)))])
	    (process!-run-while process! (lambda ()
					   (and (not-reached-fix-point?)
						(not-reached-time?))))))))

(define (make-is-fixed-point? step0! changed0? step1! changed1?)
  (with (step!observes->wrap-with-fold
	 (lambda (ind0-ind1 index ch)
	   (if ch
	       (list #f #f)
	       (with ind0-ind1
		     (ind0 ind1)
		     (if (= index 0)
			 (list #t ind1)
			 (list ind0 #t)))))
	 (list #f #f)
	 (list step0! changed0?)
	 (list step1! changed1?))
	(ind0-ind1 step0! step1!)
	(list (lambda ()
		(andmap (lambda (x) x)
			(ind0-ind1)))
	      step0!
	      step1!)))

(define (duration->reached-duration? duration)
  (let ([start (current-time 'time-monotonic)])
    (lambda ()
      (time>=? (time-difference (current-time 'time-monotonic) start)
	      duration))))

(define (process!-run-clocked duration process!)
  (let ([start (current-time 'time-monotonic)])
    (letrec ([loop (lambda (process! time)
		     (if (time<? (time-difference time start)
				 duration)
			 (loop (process!) (current-time 'time-monotonic))))])
      (loop process! start))))
