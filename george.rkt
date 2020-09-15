(define make-vect cons)
(define xcor car)
(define ycor cdr)
(define make-segment list)
(define start-segment first)
(define end-segment second)

(define p1 (make-vect .25 0))
(define p2 (make-vect .35 .5))
(define p3 (make-vect .3 .6))
(define p4 (make-vect .15 .4))
(define p5 (make-vect 0 .65))
(define p6 (make-vect .4 0))
(define p7 (make-vect .5 .3))
(define p8 (make-vect .6 0))
(define p9 (make-vect .75 0))
(define p10 (make-vect .6 .45))
(define p11 (make-vect 1 .15))
(define p12 (make-vect 1 .35))
(define p13 (make-vect .75 .65))
(define p14 (make-vect .6 .65))
(define p15 (make-vect .65 .85))
(define p16 (make-vect .6 1))
(define p17 (make-vect .4 1))
(define p18 (make-vect .35 .85))
(define p19 (make-vect .4 .65))
(define p20 (make-vect .3 .65))
(define p21 (make-vect .15 .6))
(define p22 (make-vect 0 .85))

(define george-lines
  (list (make-segment p1 p2)
        (make-segment p2 p3)
        (make-segment p3 p4)
        (make-segment p4 p5)
        (make-segment p6 p7)
        (make-segment p7 p8)
        (make-segment p9 p10)
        (make-segment p10 p11)
        (make-segment p12 p13)
        (make-segment p13 p14)
        (make-segment p14 p15)
        (make-segment p15 p16)
        (make-segment p17 p18)
        (make-segment p18 p19)
        (make-segment p19 p20)
        (make-segment p20 p21)
        (make-segment p21 p22)
        )
  )

(define make-rectangle list)
(define origin first)
(define x-axis second)
(define y-axis third)

(define (+vect v1 v2)
  (make-vect (+(xcor v1)(xcor v2))
             (+(ycor v1)(ycor v2))))


(define (scale-vect vect factor)
  (make-vect (* factor (xcor vect))
             (* factor (ycor vect))))

(define (-vect v1 v2)
  (+vect v1 (scale-vect v2 -1)))

(define (rotate-vect v angle)
  (let ((c (cos angle))
        (s (sin angle)))
    (make-vect (- (* c (xcor v))
                  (* s (ycor v)))
               (+ (* c (ycor v))
                  (* s (xcor v))))))

(define (coord-map rect)
  (lambda (p)
    (+vect (origin rect)
           (+vect (scale-vect (x-axis rect) (xcor p))
                  (scale-vect (y-axis rect) (ycor p)))
           )
    )
  )

(define frame (new frame% [label "Paint George"]
                   [width 320]
                   [height 340])
  )

(define canvas (new canvas% [parent frame]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc set-pen red-pen)
                       (send dc set-brush no-brush)
                       (on-paint))
                     ])
  )

(define red-pen (make-object pen% "RED" 2 'solid))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define dc (send canvas get-dc))

(define (make-picture seglist)
  (lambda (rect)
    (for-each
     (lambda (segment)
       (let* ((b (start-segment segment))
              (e (end-segment segment))
              (m (coord-map rect))
              (b2 (m b))
              (e2 (m e)))
       (send dc draw-line (xcor b2) (ycor b2) (xcor e2) (ycor e2))))
     seglist)))

(define george (make-picture george-lines))

(define (rotate90  pict)
  (lambda (rect)
    (pict (make-rectangle
           (+vect (origin rect)
                  (x-axis rect))
           (y-axis rect)
           (scale-vect (x-axis rect) -1)))))

(define (together pict1 pict2)
  (lambda (rect)
    (pict1 rect)
    (pict2 rect)))

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

(define big-brother
  (beside george
          (above empty-picture george .5) 
          .5)
  )

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

(define acrobats
  (beside george
          (rotate180 (flip george)) .5))

(define 4bats
  (above acrobats
         (flip acrobats)
         .5))

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

(define (corner-push pict n)
  (if (= n 0)
      pict
      (above
       (beside
        pict
        (right-push pict (- n 1))
        .75)
       (beside
        (up-push pict n)
        (corner-push pict (- n 1))
        .75)
       .75))
  )

(define (4pict p1 r1 p2 r2 p3 r3 p4 r4)
  (beside
   (above
    ((repeated rotate270 r2) p2)
    ((repeated rotate90 r1) p1)
    .5)
   (above
    ((repeated rotate270 r4) p4)
    ((repeated rotate90 r3) p3)
    .5)
   .5)
  )

(define (4same p r1 r2 r3 r4)
  (4pict p r1 p r2 p r3 p r4)
  )

(define (square-limit pict n)
  (4same (corner-push pict n)
         3 2 0 3))

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

(define george (rotate180 (flip george)))
(define big-brother (rotate180 (flip big-brother)))
(define acrobats (rotate180 (flip acrobats)))
(define 4bats (rotate180 (flip 4bats)))

(define p23 (make-vect 0 0))
(define p24 (make-vect 1 0))
(define p25 (make-vect 0 1))
(define p26 (make-vect 1 1))

(define frame-line
  (list (make-segment p23 p24)
        (make-segment p23 p25)
        (make-segment p24 p26)
        (make-segment p25 p26)
        )
  )

(define frame_line (make-picture frame-line))

(define (on-paint) ((together george (rotate270 george)) frame1)
  (4bats frame2)
  ((corner-push 4bats 2) frame3)
  (big-brother frame4)
  ((up-push george 3) frame5)
  ((4same george 0 1 2 3) frame6)
  (acrobats frame7)
  ((right-push george 2) frame8)
  ((square-limit 4bats 2) frame9)
  (frame_line frame1)
  (frame_line frame2)
  (frame_line frame3)
  (frame_line frame4)
  (frame_line frame5)
  (frame_line frame6)
  (frame_line frame7)
  (frame_line frame8)
  (frame_line frame9)
  )

(send frame show #t)