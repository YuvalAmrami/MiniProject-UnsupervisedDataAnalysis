(load "libs/io.scm")

(define (make-cache-set-get)
  (let ([cached? #f]
	[cached-value 'unrecorded])
    (list (lambda (x)
	    (set! cached? #t)
	    (set! cached-value x))
	  (lambda ()
	    (if cached?
		cached-value
		(error 'get-cache "did not cache any value yet"))))))

(define (compute->cashed-getter compute)
  (let ([cached? #f]
	[cached-value 'uncached])
    (lambda ()
      (if (not cached?)
	  (begin
	    (set! cached-value (compute))
	    (set! cached? #t)))
      cached-value)))

(define (store-name-compute->store-based-cache-getter store-name compute)
  (let ([cached? #f]
	[cached-value 'uncached])
    (lambda ()
      (if (not cached?)
	  (if (io-exists? store-name)
	      (begin
		(set! cached-value (io-read store-name))
		(set! cached? #t))
	      (let ([x (compute)])
		(io-write store-name x)
		(set! cached-value x)
		(set! cached? #t))))
      cached-value)))

(define (cache-share-map f get)
  (lambda ()
    (f (get))))

 (define (cache-map f get)
   (compute->cashed-getter
    (lambda ()
      (f (get)))))

(define (cache-storage-map name f get)
  (store-name-compute->store-based-cache-getter
   name (lambda ()
	  (f (get)))))
