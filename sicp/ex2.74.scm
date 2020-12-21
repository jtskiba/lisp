;;ex2.74
;;1)

(define (make-generic division file-specific)
  (cons divison file-specific))

(define (which-division file-generic)
  (car file-generic))

(define (which-file file-generic)
  (cadr file-generic))

(define (get-record name file-generic)
  (let (
	(divis (which-division file-generic))
	(file (which-file file-generic))
	)
    ((get 'get-record divis) name file)))

;;2)

(define (get-salary file-generic)
  (let (
	(divis (which-division file-generic))
	(file (which-file file-generic))
	)
    ((get 'get-salary divis) file)))

;;3)

(define (find-employee-record name list-of-files)
  (cond ((null? list-of-files) (error 'No such employee in record))
	((get-record name (car list-of-files))
	 (get-record name (car list-of-files)))
	(else (find-employee-record name (cdr list-of-files)))))


	 


	      
