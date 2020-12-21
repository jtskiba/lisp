#lang racket
;from sicp
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key 
                 (key (car set-of-records)))
         (car set-of-records))
        (else 
         (lookup given-key 
                 (cdr set-of-records)))))

(define (lookup-tree given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup-tree 
          given-key 
          (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup-tree 
          given-key 
          (right-branch set-of-records)))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))


;ex2.66
(define (data record) (cdr record))
(define (key record) (car record))
(define (make-record key data) (cons key data))

(define database
  (list (make-record 1 "Bogdan")
        (make-record 2 "Zofia")
        (make-record 3 "Marek")
        (make-record 4 "Kaska")
        (make-record 5 "Zbych")
        (make-record 6 "Aska")
        (make-record 7 "Maryska")
        (make-record 8 "Franek")
        (make-record 9 "Zenek")))


(define database-tree (list->tree database))

(lookup 7 database)
(lookup-tree 7 database-tree)

;database
;database-tree

