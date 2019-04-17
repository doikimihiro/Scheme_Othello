;
; array functions for scheme
;

;
; (make-array prototype n m) or
; (make-array prototype '(n1 n2) '(m1 m2))
;
; an array is represented in the form (n1 m1 'vector of vectors')
;
(define (make-array proto r c)
  (let ((x1 (if (integer? r) 0 (car r)))
	(x2 (+ (if (integer? r) r (cadr r)) 1))
	(y1 (if (integer? c) 0 (car c)))
	(y2 (+ (if (integer? c) c (cadr c)) 1)))
    (do ((m (make-vector (- y2 y1)))
	 (i y1 (+ i 1)))
	((= i y2) (list x1 y1 m))
      (vector-set! m (- i y1) (make-vector (- x2 x1) proto )))))

;
; (array-set! array value x-pos y-pos)
;
; returns the new value.
;
(define (array-set! a v x y)
  (vector-set! (vector-ref (caddr a) (- y (cadr a))) (- x (car a)) v)
  v)

;
; (array-ref array x-pos y-pos)
;
(define (array-ref a x y)
  (vector-ref (vector-ref (caddr a) (- y (cadr a))) (- x (car a))))

;
; (array-in-bounds? array x-pos y-pos
;
; returns #t if the position is in the range of the array.
;
(define (array-in-bounds? a x y)
  (cond	((< y (cadr a)) #f)
	((< x (car a)) #f)
	((<= (vector-length (caddr a)) (- y (cadr a))) #f)
	((<= (vector-length (vector-ref (caddr a) (- y (cadr a))))
	     (- x (car a))) #f)
	(else #t)))
