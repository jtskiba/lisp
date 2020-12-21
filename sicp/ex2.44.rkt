
;#lang sicp

#lang racket

;;;;;;;;;;;;;;;;;;INTERNAL DEF's for PICTURE LANGUAGE ;;;;;;;;;;;;;;
; thanks to:
; http://notebook.xyli.me/SICP/SICP-with-DrRacket/

;(#%require sicp-pict)
 (require sicp-pict)

;;;;;;;;;;;;;;;;;;;;;;;;TESTING;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(paint mark-of-zorro)
;(paint black) ; white and gray also available
;(paint diagonal-shading)

;;;;;;;;;;;;;FROM BOOK;;;;;;;;;;;;

;(define pic1 (beside einstein (flip-vert einstein)))
;(define pic2 (below pic1 pic1))

;(paint pic1)
;(paint pic2)
;(paint einstein)
;(paint (flip-horiz einstein))

(define (flipped-pairs painter)
  (let ((painter2 
         (beside painter 
                 (flip-vert painter))))
    (below painter2 painter2)))

;(define pic4 (flipped-pairs einstein))
;(paint pic4)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

;(define pic5a (right-split einstein 0))
;(paint pic5a)

;(define pic5b (right-split einstein 1))
;(paint pic5b)

;(define pic5c (right-split einstein 2))
;(paint pic5c)


;(define pic5d (right-split einstein 3))
;(paint pic5d)


(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                                  (- n 1))))
        (below painter 
                (beside smaller smaller)))))

;(define pic6a (up-split einstein 0))
;(paint pic6a)

;(define pic6b (up-split einstein 1))
;(paint pic6b)

;(define pic6c (up-split einstein 2))
;(paint pic6c)

;(define pic6d (up-split einstein 3))
;(paint pic6d)


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

;(define pic7a (corner-split einstein 0))
;(paint pic7a)

;(define pic7b (corner-split einstein 1))
;(paint pic7b)

;(define pic7c (corner-split einstein 2))
;(paint pic7c)

;(define pic7d (corner-split einstein 3))
;(paint pic7d)

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))

;(define pic8a (square-limit einstein 4))
;(paint pic8a)


;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ex2.44;;;;;;;;;;;;;
'(ex2.44);;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;Already done above (i.e. design of up-split procedures
; for completeness added below 


;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (up-split painter 
;                                  (- n 1))))
;        (below painter 
;                (beside smaller smaller)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Higher-order operations;;;;;
'(Higher-order operations);;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



