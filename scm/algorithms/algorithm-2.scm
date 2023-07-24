(load "libs/macros.scm")
(load "libs/lib.scm")

"
general:

We have Ass partition and Bss partition.
Then foreach As in Ass we calculate a centroid where
Bss is the axis system.
Then we recalculate Ass to be the set of all As(c) for all centroids c
where for centroid c, As(c) is the set of all A whose closest centroid is c.

deeper dive:

BCoords :
 It is a list of vectors of length Bc
 where for each BCoord of them whose position is i
            and B we get
   (vector-ref BCoord B) is the ammount of As B is related to.
   B's position in the BCoords axis system is defined as:
     (map (lambda (BCoord)
             (vector-ref BCoord B))
          BCoords)

Ais : a list of one of [0 1 ... (- BCoordc 1)] of length Ac
 It is a list of As' cluster indecies where for each entry of index A,
 its value is the cluster A belongs to.

ABss :
  It is a list of Bss where each Bs element whose index is A,
  are all the Bs related to A.
"


(define (step! Ais BCoords ABss ACoords)
  (recluster! (Ais->centroids Ais (length BCoords) ACoords)
	      Ais BCoords ACoords ABss))

(define (recluster! centroids-vec Ais BCoords ACoords ABss)
  (letrec ([loop (lambda (A Ais ABss)
		   (let* ([i (car Ais)]
			  [j (closest-centroid (A->position ACoords A) centroids-vec)]
			  [BCoord-i (list-ref BCoords i)]
			  [BCoord-j (list-ref BCoords j)]
			  [ABs (car ABss)])
		     (if (not (= i j))
			 (begin
			   (for-each (lambda (B)
				       (vector-fset! BCoord-i B (lambda (bc) (- bc 1)))
				       (vector-fset! BCoord-j B (lambda (bc) (+ bc 1))))
				     ABs)
			   (set-car! Ais j))))
		   (if (not (null? (cdr Ais)))
		       (loop (+ A 1) (cdr Ais) (cdr ABss))))])
    (loop 0 Ais ABss)))

(define (closest-centroid pos cs-vec)
  (let ([cc (vector-length cs-vec)])
    (letrec ([loop (lambda (i i-min d^2-min)
		     (if (>= i cc)
			 i-min
			 (let ([d^2 (dist^2 pos (vector-ref cs-vec i))])
			   (if (< d^2 d^2-min)
			       (loop (+ i 1) i d^2)
			       (loop (+ i 1) i-min d^2-min)))))])
      (loop 1 0 (dist^2 pos (vector-ref cs-vec 0))))))

(define (Ais->centroids Ais BCoordc ACoords)
  (let ([centroids-vec (make-vector BCoordc)])
    (letrec ([init!
	      (lambda (i)
		(if (< i BCoordc)
		    (begin
		      (vector-set! centroids-vec i
				   (list (make-list (length ACoords) 0)
					 0))
		      (init! (+ i 1)))))]
	     [add-up-Ais!
	      (lambda (Ais A)
		(if (not (null? Ais))
		    (begin
		      (vector-set! centroids-vec (car Ais)
				   (with (vector-ref centroids-vec
						     (car Ais))
					 (s c) (list (map + s (A->position ACoords A))
						     (+ c 1))))
		      (add-up-Ais! (cdr Ais) (+ A 1)))))]
	     [sums->averages!
	      (lambda (i)
		(if (< i BCoordc)
		    (begin
		      (vector-set! centroids-vec i
				   (with (vector-ref centroids-vec i)
					 (s c) (if (> c 0)
						   (map (lambda (s_) (/ s_ c))
							s)
						   s)))
		      (sums->averages! (+ i 1)))))])
      (init! 0)
      (add-up-Ais! Ais 0)
      (sums->averages! 0))
    centroids-vec))

(define (A->position ACoords A)
  (map (lambda (ACoord)
	 (vector-ref ACoord A))
       ACoords))

(define (dist^2 pos1 pos2)
  (apply +
	 (map (lambda (x) (* x x))
	      (map - pos1 pos2))))


