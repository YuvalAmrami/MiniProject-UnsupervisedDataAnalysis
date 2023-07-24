(load "libs/macros.scm")
(load "libs/io.scm")
(load "libs/lib.scm")
(load "libs/parse.scm")

(define (parse-lines line)
  (parse-must-end
   (parse-map
    (lambda (x _) x)
    (parse-interleave-ignored
     line
     (parse-plus (parse-a #\newline)))
    (parse-star (lambda (lst) (line-seperator lst))))))

(define parse-u.m-s
  (parse-lines (lambda (lst) (u.m lst))))

(define parse-mId-title-s
  (parse-lines (lambda (lst) (mId-title lst))))

(define line-seperator (parse-a #\newline))
(define word-seperator
  (parse-map (lambda (x y) 'word-seperator)
	     (parse-a #\:)
	     (parse-a #\:)))
(define word-char
  (parse-add
   (parse-by (lambda (x) (and (not (eq? x #\newline))
			      (not (eq? x #\:))))
	     'simple-word-char)
   (parse-not-followed-by
    #\: (parse-a #\:)
    #\: (parse-a #\:))))
(define numeral-char
  (parse-by (lambda (c)
	      (and (char<=? #\0 c)
		   (char<=? c #\9)))
	    'numeral-char))
(define string-word
  (parse-map (lambda (cs)
	       (apply string cs))
	     (parse-star (lambda (lst) (word-char lst)))))
(define numeral-string-word
  (parse-map (lambda (cs)
	       (apply string cs))
	     (parse-plus (lambda (lst) (numeral-char lst)))))
(define number-word
  (parse-map string->number
	     (lambda (lst) (numeral-string-word lst))))
(define u.m
  (parse-map
   (lambda (u _ m . __)
     (cons u m))
   (lambda (lst) (number-word lst)) word-seperator (lambda (lst) (number-word lst)) word-seperator
   (lambda (lst) (string-word lst)) word-seperator (lambda (lst) (string-word lst))))

(define mId-title
  (parse-map
   (lambda (id _ title . __)
     (list id title))
   number-word word-seperator
   string-word word-seperator
   string-word))

(define string-split
  (lambda (sep s)
    (let ((next-i (lambda (i) (+ i 1)))
	  (end?-i (let ([len (string-length s)])
		    (lambda (i) (>= i len))))
	  (init-i 0)
	  (char-i (lambda (i) (string-ref s i))))
      (letrec ((word (lambda (i nothing something)
		       (if (or (end?-i i) (eq? (char-i i) sep))
			   (nothing i)
			   (word (next-i i)
				 (lambda (j)
				   (something (string (char-i i))
					      j))
				 (lambda (res j)
				   (something (string-append (string (char-i i)) res)
					      j))))))
	       (words (lambda (i)
			(if (end?-i i)
			    '()
			    (word i
				  (lambda (i) (words (next-i i)))
				  (lambda (w i)
				    (cons w (words i))))))))
	(words init-i)))))

(define string->ratings
  (lambda (s)
    (map (lambda (s)
	   (map string->number (string-split #\: s)))
	 (string-split #\newline s))))

(define string->movie-id--title
  (lambda (s)
    (parse-result-dispatch
     (parse-mId-title-s (string->list s))
     (lambda (x _) x)
     (lambda (err) (error 'string->movie-id--title err)))
    #;
    (map (lambda (s)
	   (let ([splitted (string-split #\: s)])
	     (if (= (length splitted) 3)
		 (with splitted
		       (movieid title _genres)
		       (list (string->number movieid)
			     title))
		 (display `(error ,splitted)))))
	 (string-split #\newline s))))

(define ratings->u.m
  (lambda (ratings)
    (map (lambda (rating)
	   (with rating
		 (a b . _)
		 (cons a b)))
	 ratings)))

(define filter-u.m
  (letrec ([u.ms->u.m a.bs->a.b]
	   [m.us->u.m (lambda (m.us)
			(a.b->b.a (a.bs->a.b m.us) = < <))]
	   [filter-u.m-u (lambda (u.m u-limit)
			   (u.ms->u.m
			    (filter (lambda (u-ms)
				      (>= (length (cdr u-ms)) u-limit))
				    (u.m->u.ms u.m))))]
	   [filter-u.m-m (lambda (u.m m-limit)
			   (m.us->u.m
			    (filter (lambda (m-us)
				      (>= (length (cdr m-us)) m-limit))
				    (u.m->m.us u.m))))]
	   [loop (lambda (u.m u-limit m-limit)
		   (let ([u.m_ (filter-u.m-m (filter-u.m-u u.m
							   u-limit)
					     m-limit)])
		     (if (< (length u.m_) (length u.m))
			 (loop u.m_ u-limit m-limit)
			 u.m)))])
    loop))

(define (raw-ratings-file->u.m file-name)
  (ratings->u.m
   (string->ratings
    (get-string-all
     (open-file-input-port file-name
			   (file-options)
			   (buffer-mode none)
			   (make-transcoder (utf-8-codec)))))))

(define u.m-><u.m-cont>-<maps-u-m>
  (lambda (u.m)
    (list (u.m->u.m-continuous u.m)
	  (list->vector (u.m->us u.m))
	  (list->vector (u.m->ms u.m)))))

(define (raw-movies-file->movie-title-list file-name)
  (sort (lambda x (apply < (map car x)))
	(string->movie-id--title
	 (get-string-all
	  (open-file-input-port file-name
				(file-options)
				(buffer-mode none)
				(make-transcoder (utf-8-codec)))))))
