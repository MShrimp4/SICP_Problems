#lang sicp
(#%require sicp-pict) ;use einstein!
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(define (up-split painter n)
  (if (= n 0)
     painter
     (let ((smaller (up-split painter (- n 1))))
       (below painter (beside smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
(paint (corner-split einstein 5))
(paint (square-limit einstein 5))
(define (split orig next);2.45
  (define (temp painter n)
    (if (= n 0)
       painter
       (let ((smaller (temp painter (- n 1))))
         (orig  painter (next smaller smaller)))))
  temp)
;2.46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)
(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v a)
  (make-vect
   (* (xcor-vect v) a)
   (* (ycor-vect v) b)))
;2.47
(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))
(define origin-f1 car)
(define edge1-f1 cadr)
(define edge2-f1 caddr)

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define origin-f2 car)
(define edge1-f2 cadr)
(define edge2-f2 cddr)
;2.48
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
;2.49
(define (outline frame);I`ll just do this and leave the rest
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 0 1))
    (make-segment (make-vect 0 0) (make-vect 1 0))
    (make-segment (make-vect 0 1) (make-vect 1 1))
    (make-segment (make-vect 1 0) (make-vect 1 1)))))
;2.50
(define (flip-horizon painter)
  (transform-painter
   painter
   (make-vect 1 0)
   (make-vect 0 0)
   (make-vect 1 1)))
(define (rotate180 painter)
  (transform-painter
   painter
   (make-vect 1 1)
   (make-vect 0 1)
   (make-vect 1 0)));no rotate270. I`m lazy
;2.51
(define (below p1 p2)
  (let ((splint-point (make-vect 0 0.5)))
    (let ((under (transform-painter
                    p1
                    (make-vect 0 0)
                    (make-vect 1 0)
                    splint-point))
          (upper (transform-painter
                    p2
                    splint-point
                    (make-vect 1 0.5)
                    (make-vect 1 1))))
      (lambda (frame)
        (upper frame)
        (lower frame)))))
(define (below2 p1 p2)
  (rotate90 (beside (rotate270 p1) (rotate270 p2))))
;2.52 - skipped. NAH I`M LAZY