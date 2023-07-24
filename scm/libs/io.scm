(define (name->path name)
  (string-append "fasl/" name ".fasl"))

(define (io-read name)
  (fasl-read (open-file-input-port (name->path name))))

(define (io-write name value)
  (fasl-write value
	      (open-file-output-port (name->path name)
				     (file-options no-fail))))

(define (io-exists? name)
  (file-exists? (name->path name)))
