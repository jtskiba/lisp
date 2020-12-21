#lang racket
;from sicp Huffman Encoding trees
(define (entry tree) (car tree))

(define (make-tree entry left right)
  (list entry left right))


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

;adjoin assuming ordered set
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol1 symbol tree) ;without error message
  (if (leaf? tree)
      '()
      (if (element-of-set? symbol (symbols (left-branch tree)))
          (cons 0 (encode-symbol1 symbol (left-branch tree)))
          (cons 1 (encode-symbol1 symbol (right-branch tree))))))

(define (encode-symbol symbol tree) ;with error messages
  (if (leaf? tree)
      '()
      (cond ((element-of-set? symbol (symbols (left-branch tree)))
             (cons 0 (encode-symbol symbol (left-branch tree))))
            ((element-of-set? symbol (symbols (right-branch tree)))
             (cons 1 (encode-symbol symbol (right-branch tree))))
            (else (error "bad symbol, not in tree: 
               SYMBOL" symbol)))))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (adjoin-set2 x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set2 x (cdr set)))))) 
         
(define (successive-merge leaves)
  (if (<= (length leaves) 1)
      (car leaves)
      (successive-merge
       (adjoin-set2       
        (make-code-tree
         (car leaves)  ;(make-leaf-set my_pairs)
         (cadr leaves));(make-leaf-set my_pairs)
        (cddr leaves)))))



;ex2.69
(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define my_pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define my-tree (generate-huffman-tree my_pairs))

(define my-message
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP 
        YIP YIP YIP YIP YIP
        SHA BOOM))



(define my-code (encode my-message my-tree))
(length my-code) ;84 bits required to encode this message using Huffman generating trees
(decode my-code my-tree)
;if we use 8 symbol alphabet we neeg log2 of 8 = 3 bits for each letter. In the message there are 89 letters (excl spaces)
; hence would eed 89*3 = 267 bits

;ex2.71
;most frequent symbol will be assigned to 0 or 1, so 1 bit. that is regardless if the n=5 or n=10
;least frequent symbol will be assigned to n-1 bits, that is therefore 4 and 9 bits for n=5 and n=10 respectively

