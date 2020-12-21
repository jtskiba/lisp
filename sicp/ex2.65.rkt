#lang racket
;from sicp - latest - sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))


(define tree1  '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2  '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3  '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))



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

;(list->tree '(1 3 5 7 9 11))

;ex2.65
; the implmentationsof union and intersect in 2.59 were poor and gave n^2 order of growth
; the implementation of union & intersect in 2.62 was much better and gave n order of growth
; now to do union and intersect on balanced tree, perform the following:
;   -> Convert the balanced binary trees to ordered lists.
;   -> Perform the desired operation (union-set or intersection-set).
;   -> Convert the resulting ordered set back to a balanced binary tree.

; union and intersect at order growth of n for ordered 
(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        ((and (null? set1) (null? set2)) '())
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        ((= (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) (cdr set2))))
        (else (cons (car set2) (union-set set1 (cdr set2))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set 
                          set1 
                          (cdr set2)))))))

;check if both work ok - works fine!
;(union-set '(1 2 3) '(2 3 4 5))
;(intersection-set '(1 2 3) '(2 3 4 5))

; so now we are missing the following:
; Convert the balanced binary trees to ordered lists.

(define (intersect tree1 tree2)
  (list->tree
   (intersection-set
    (tree->list-1 tree1)
    (tree->list-1 tree2))))

(define (unite tree1 tree2)
  (list->tree
   (union-set
    (tree->list-1 tree1)
    (tree->list-1 tree2))))


(define treeA '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define treeB '(3 (1 () ()) (7 (5 () ()) (9 () (13 () ())))))

(intersect treeA treeB)
(unite treeA treeB)



