(load "libs/io.scm")

(define vec-m-p (io-read "vec-m-p"))
(define vec-m-m-p (io-read "vec-m-m-p"))
(define ms (io-read "ms"))

(define movies
  (with-input-from-file "movies.txt"
    (lambda ()
      (letrec ([while (lambda (r res)
			(if (eof-object? r)
			    (reverse res)
			    (while (read) (cons r res))))])
	(while (read) '())))))

(define (m->p m)
  (vector-ref vec-m-p m))
(define (mm->p m1 m2)
  (if (< m1 m2)
      (vector-ref (vector-ref vec-m-m-p m1) (- m2 m1 1))
      (mm->p m2 m1)))
