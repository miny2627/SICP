(define make-vect cons)
(define xcor car)
(define ycor cdr)

(define make-segment list)
(define start-segment first)
(define end-segment second)

(define make-rectangle list)
(define origin first)
(define x-axis second)
(define y-axis third)

(define (make-picture seglist)
  (lambda (rect)
    (for-each
     (lambda (segment)
       (let*((b (start-segment segment))
             (e (end-segment segment))
             (m (coord-map rect))
             (b2 (m b))
             (e2 (m e)))
         (send dc draw-line (xcor b2) (ycor b2) (xcor e2) (ycor e2))))
         seglist)))

(define p1 (make-vect 0 0))
(define p2 (make-vect 0 1))
(define p3 (make-vect 1 0))
(define p4 (make-vect 1 1))

(define triangle-lines
  (list (make-segment p1 p4)
        (make-segment p1 p2)
        (make-segment p1 p3)
        (make-segment p2 p3)
        (make-segment p2 p4)
        (make-segment p3 p4)
        )
  )

(define triangle (make-picture triangle-lines))

(define (+vect v1 v2)
  (make-vect (+(xcor v1)(xcor v2))
             (+(ycor v1)(ycor v2))))


(define (scale-vect vect factor)
  (make-vect (* factor (xcor vect))
             (* factor (ycor vect))))

(define (-vect v1 v2)
  (+vect v1 (scale-vect v2 -1)))

(define (coord-map rect)
  (lambda (p)
    (+vect (origin rect)
           (+vect (scale-vect (x-axis rect) (xcor p))
                  (scale-vect (y-axis rect) (ycor p)))
           )
    )
  )

(define (rotate90  pict)
  (lambda (rect)
    (pict (make-rectangle
           (+vect (origin rect)
                  (x-axis rect))
           (y-axis rect)
           (scale-vect (x-axis rect) -1)))))

(define (beside pict1 pict2 a)
  (lambda (rect)
    (pict1
     (make-rectangle
      (origin rect)
      (scale-vect (x-axis rect) a)
      (y-axis rect)))
    (pict2
     (make-rectangle
      (+vect
       (origin rect)
       (scale-vect (x-axis rect) a))
      (scale-vect (x-axis rect) (- 1 a))
      (y-axis rect)))))

(define (above pict1 pict2 a)
  ((repeated rotate90 3)
   (beside (rotate90 pict1)
           (rotate90 pict2)
           a))
  )

(define (repeated f n)
  (lambda (t)
    (if (= n 0)
        t
        ((repeated f (- n 1)) (f t)))))

(define empty-picture (make-picture '()))
(define (flip pict)
  (lambda (rect)
    (pict (make-rectangle
           (+vect (origin rect) (x-axis rect))
           (scale-vect (x-axis rect) -1)
           (y-axis rect)))
    )
  )

(define rotate180 (repeated rotate90 2))
(define rotate270 (repeated rotate90 3))

(define (up-push pict n)
  (if (= n 0)
      pict
      (above pict
             (up-push pict (- n 1))
             .75))
  )

(define (right-push pict n)
  (if (= n 0)
      pict
      (beside pict
              (right-push pict (- n 1))
              .75))
  )

(define acrobats
  (beside triangle
          (rotate180 (flip triangle)) .5))

(define 4bats
  (above acrobats
         (flip acrobats)
         .5))

(define (up-push pict n)
  (if (= n 0)
      pict
      (above pict
             (up-push pict (- n 1))
             .5))
  )

(define (right-push pict n)
  (if (= n 0)
      pict
      (beside pict
              (right-push pict (- n 1))
              .5))
  )

(define (corner-push1 pict n)
  (if (= n 0)
      pict
      (above
       (beside
        (corner-push1 pict (- n 1))
        (up-push pict n)
        .65)
       (beside
        (corner-push1 pict (- n 1))
        (up-push pict n)
        .65)
       .5)
      )
  )

(define (corner-push2 pict n)
  (if (= n 0)
      pict
      (above
          (beside
           (beside triangle triangle .5)
           triangle .65
           )
       (beside
        (corner-push1 pict (- n 1))
        (up-push pict n)
        .65)
       .35)
      )
  )

(define triangle (rotate180 (flip triangle)))
(define acrobats (rotate180 (flip acrobats)))
(define 4bats (rotate180 (flip 4bats)))

(define origin1 (make-vect 0 0))
(define x-axis1 (make-vect 100 0))
(define y-axis1 (make-vect 0 100))

(define frame1
  (make-rectangle origin1
                  x-axis1
                  y-axis1))

(define origin2 (make-vect 100 0))
(define frame2
  (make-rectangle origin2 x-axis1 y-axis1))

(define origin3 (make-vect 200 0))
(define frame3
  (make-rectangle origin3 x-axis1 y-axis1))

(define origin4 (make-vect 0 100))
(define frame4
  (make-rectangle origin4 x-axis1 y-axis1))

(define origin5 (make-vect 100 100))
(define frame5
  (make-rectangle origin5 x-axis1 y-axis1))

(define origin6 (make-vect 200 100))
(define frame6
  (make-rectangle origin6 x-axis1 y-axis1))

(define origin7 (make-vect 0 200))
(define frame7
  (make-rectangle origin7 x-axis1 y-axis1))

(define origin8 (make-vect 100 200))
(define frame8
  (make-rectangle origin8 x-axis1 y-axis1))

(define origin9 (make-vect 200 200))
(define frame9
  (make-rectangle origin9 x-axis1 y-axis1))

(define frame (new frame% [label "Paint Triangle"]
                   [width 330]
                   [height 340]))

(define canvas (new canvas% [parent frame]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc set-pen red-pen)
                       (send dc set-brush no-brush)
                       (on-paint))]))
(define red-pen (make-object pen% "RED" 2'solid))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define dc (send canvas get-dc))

(define (on-paint) ((rotate180  (corner-push2 triangle 1))  frame1)
  ((rotate270 (corner-push1 triangle 1)) frame2)
  ((rotate270 (corner-push2 triangle 1)) frame3)
  ((rotate180 (corner-push1 triangle 1)) frame4)
  (triangle frame5)
  ((corner-push1 triangle 1) frame6)
  ((rotate90 (corner-push2 triangle 1)) frame7)
  ((rotate90 (corner-push1 triangle 1)) frame8)
  ((corner-push2 triangle 1) frame9)
  )

(send frame show #t)
