;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname FinalProject-8x8-03) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;FinalProject : Melody Maker

;a World State is make-ws1
;where (make-ws1) is (make-ws1 w1 w2 w3 ... w8 timer timer2)
;where each number corresponds to its respective columm
;and timer is the time of the music playing
;From w5 --> w8, it will be the second "window"
;timer corresponds to the first window, while timer2 is with the second.

;;;Temporarily w1 w2 w3...-> w8 will be taking in booleans, but if trouble
;;;arises, we can change the booleans to a list of booleans, or something.

;8x8. Each "window" will be 4 x 8 (row x column)
(define-struct ws1 [w1 w2 w3 w4 w5 w6 w7 w8 timer timer2])

;Drawn representation of whether a note is going to be played
(define RECT-HEIGHT 29)
(define RECT-WIDTH 150)

(define rect1 (rectangle RECT-WIDTH RECT-HEIGHT "solid" "red"))
(define rect2 (rectangle RECT-WIDTH RECT-HEIGHT "solid" "orange"))
(define rect3 (rectangle RECT-WIDTH RECT-HEIGHT "solid" "yellow"))
(define rect4 (rectangle RECT-WIDTH RECT-HEIGHT "solid" "green"))
(define rect5 (rectangle RECT-WIDTH RECT-HEIGHT "solid" "turquoise"))
(define rect6 (rectangle RECT-WIDTH RECT-HEIGHT "solid" "blue"))
(define rect7 (rectangle RECT-WIDTH RECT-HEIGHT "solid" "purple"))
(define rect8 (rectangle RECT-WIDTH RECT-HEIGHT "solid" "magenta"))

;;
(define BLANK-WIDTH 1275)
(define BLANK-HEIGHT 500)
(define blank (rectangle BLANK-WIDTH BLANK-HEIGHT "solid" "white"))
(define SLIDER (rectangle 10 (+ (/ RECT-HEIGHT 2) (- BLANK-HEIGHT 3.5)) "solid" "gray"))

;Drawn representation of 'silence'
(define dot (circle 3 "solid" "gray"))

;Exists purely for aesthetics
(define dividers (rectangle 6 (+ (/ RECT-HEIGHT 2) (- BLANK-HEIGHT 3.5)) "solid" "white"))

;This will be the border to divide the two "windows"
;Or the border below the two windows
(define border (rectangle 4 (+ (/ RECT-HEIGHT 2) (- BLANK-HEIGHT 3.5)) "solid" "black"))
(define bot-border (rectangle BLANK-WIDTH 3 "solid" "white"))

(define (x-coor x)
  (cond
    [(= x 1) 75]
    [else (- (* 150 x) 75)]
    ))
    
(define (y-coor y)
  (* 30 y))

;Base-image. Also the testing period to see how it would look.
;Since this will be 8x8, there's a total of 64 dots.
(define bg-image
  (place-image dot (x-coor 1) (y-coor 1)
  (place-image dot (x-coor 2) (y-coor 1)
  (place-image dot (x-coor 3) (y-coor 1)
  (place-image dot (x-coor 4) (y-coor 1)
  (place-image dot (x-coor 5) (y-coor 1)
  (place-image dot (x-coor 6) (y-coor 1)
  (place-image dot (x-coor 7) (y-coor 1)
  (place-image dot (x-coor 8) (y-coor 1)
  (place-image dot (x-coor 1) (y-coor 2)
  (place-image dot (x-coor 2) (y-coor 2)
  (place-image dot (x-coor 3) (y-coor 2)
  (place-image dot (x-coor 4) (y-coor 2)
  (place-image dot (x-coor 5) (y-coor 2)
  (place-image dot (x-coor 6) (y-coor 2)
  (place-image dot (x-coor 7) (y-coor 2)
  (place-image dot (x-coor 8) (y-coor 2)
  (place-image dot (x-coor 1) (y-coor 3)
  (place-image dot (x-coor 2) (y-coor 3)
  (place-image dot (x-coor 3) (y-coor 3)
  (place-image dot (x-coor 4) (y-coor 3)
  (place-image dot (x-coor 5) (y-coor 3)
  (place-image dot (x-coor 6) (y-coor 3)
  (place-image dot (x-coor 7) (y-coor 3)
  (place-image dot (x-coor 8) (y-coor 3)
  (place-image dot (x-coor 1) (y-coor 4)
  (place-image dot (x-coor 2) (y-coor 4)
  (place-image dot (x-coor 3) (y-coor 4)
  (place-image dot (x-coor 4) (y-coor 4)
  (place-image dot (x-coor 5) (y-coor 4)
  (place-image dot (x-coor 6) (y-coor 4)
  (place-image dot (x-coor 7) (y-coor 4)
  (place-image dot (x-coor 8) (y-coor 4)
  (place-image dot (x-coor 1) (y-coor 5)
  (place-image dot (x-coor 2) (y-coor 5)
  (place-image dot (x-coor 3) (y-coor 5)
  (place-image dot (x-coor 4) (y-coor 5)
  (place-image dot (x-coor 5) (y-coor 5)
  (place-image dot (x-coor 6) (y-coor 5)
  (place-image dot (x-coor 7) (y-coor 5)
  (place-image dot (x-coor 8) (y-coor 5)
  (place-image dot (x-coor 1) (y-coor 6)
  (place-image dot (x-coor 2) (y-coor 6)
  (place-image dot (x-coor 3) (y-coor 6)
  (place-image dot (x-coor 4) (y-coor 6)
  (place-image dot (x-coor 5) (y-coor 6)
  (place-image dot (x-coor 6) (y-coor 6)
  (place-image dot (x-coor 7) (y-coor 6)
  (place-image dot (x-coor 8) (y-coor 6)
  (place-image dot (x-coor 1) (y-coor 7)
  (place-image dot (x-coor 2) (y-coor 7)
  (place-image dot (x-coor 3) (y-coor 7)
  (place-image dot (x-coor 4) (y-coor 7)
  (place-image dot (x-coor 5) (y-coor 7)
  (place-image dot (x-coor 6) (y-coor 7)
  (place-image dot (x-coor 7) (y-coor 7)
  (place-image dot (x-coor 8) (y-coor 7)
  (place-image dot (x-coor 1) (y-coor 8)
  (place-image dot (x-coor 2) (y-coor 8)
  (place-image dot (x-coor 3) (y-coor 8)
  (place-image dot (x-coor 4) (y-coor 8)
  (place-image dot (x-coor 5) (y-coor 8)
  (place-image dot (x-coor 6) (y-coor 8)
  (place-image dot (x-coor 7) (y-coor 8)
  (place-image dot (x-coor 8) (y-coor 8)
  blank
  )))))))))))))))))))))))))))))))))))))) ;wtf is this spawn of the paren devil
  )))))))))))))))))))))))))))

(define (w1 x) (ws1-w1 x))
(define (w2 x) (ws1-w2 x))
(define (w3 x) (ws1-w3 x))
(define (w4 x) (ws1-w4 x))
(define (w5 x) (ws1-w5 x))
(define (w6 x) (ws1-w6 x))
(define (w7 x) (ws1-w7 x))
(define (w8 x) (ws1-w8 x))
(define (wtimer x) (ws1-timer x))
(define (wtimer2 x) (ws1-timer2 x))


;;The function which deals with different states of the world
;World State --> Image
(define (image1 ws1)
  (place-image bot-border (/ BLANK-WIDTH 2) (+ (/ RECT-HEIGHT 2) (/ BLANK-HEIGHT 2) -3)
  (place-image SLIDER (* (ws1-timer ws1) BLANK-WIDTH) 0
  (place-image SLIDER (* (ws1-timer2 ws1) BLANK-WIDTH) 0
  (place-image dividers 150 0
  (place-image dividers 300 0
  (place-image dividers 450 0
  (place-image border 600 0
  (place-image dividers 600 0
  (place-image dividers 750 0
  (place-image dividers 900 0
  (place-image dividers 1050 0
    
  (place-image
   (cond
     [(w1 ws1) rect1]
     [(not (w1 ws1)) dot])
     (x-coor 1) (y-coor 1)
     
  (place-image rect2 (x-coor 2) (y-coor 2)
  (place-image rect3 (x-coor 3) (y-coor 3)
  (place-image rect4 (x-coor 4) (y-coor 4)
  (place-image rect5 (x-coor 5) (y-coor 5)
  (place-image rect6 (x-coor 6) (y-coor 6)
  (place-image rect7 (x-coor 7) (y-coor 7)
  (place-image rect8 (x-coor 8) (y-coor 8)
  bg-image
  ))))))))))))))))))))


;Draws the slider, includes the ability to loop.
;WorldState --> WorldState
(define (slider ws1)
  (make-ws1 (w1 ws1) (w2 ws1) (w3 ws1) (w4 ws1) (w5 ws1)
            (w6 ws1) (w7 ws1) (w8 ws1)
            (cond
               [(> (wtimer ws1) .465) 0]
               ;^makes it so the slider 'loops' once it reaches the end.
               [else   (+ TEMPO1 (wtimer ws1))]
               )
            ;^this is the timer for window one, which means
            ;it will affect the slider in window one
            (cond
              [(> (wtimer2 ws1) (* .47 2)) .47]
              [else   (+ TEMPO2 (wtimer2 ws1))]
              )))
  
(check-expect (slider INITIAL-STATE) (make-ws1 #t #t #t #t #t #t #t #t (+ TEMPO1 0) (+ TEMPO2 .47)))
(check-expect (slider (make-ws1 #t #t #t #t #t #t #t #t .466 1)) (make-ws1 #t #t #t #t #t #t #t #t 0 .47))

  
;;If you ever find the slider looping out of synch, try messing
;;with the 0.47. (I don't know why .47 is the way it is, but
;;trial and error led me to it.

;The tempo of either window 1 or window 2
(define TEMPO1 1/200)
(define TEMPO2 1/500)


; Handles mouse events
;WorldState Number Number MouseEvent --> World
;Will change dots/rectangles to rectangles/dots
;according to whether the user clicked on it or not.
;Otherwise, the world stays the same
;CURRENTLY A TEST.

"Is there a way to make it change only when the click happens at a certain coordinates?"

(define (mouse-test ws1 x y mouse)
  (cond
    [(and (w1 ws1) (mouse=? mouse "button-down"))
     (make-ws1 (not (w1 ws1)) (w2 ws1) (w3 ws1) (w4 ws1)
               (w5 ws1) (w6 ws1) (w7 ws1) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]
    [(and (not (w1 ws1)) (mouse=? mouse "button-down"))
     (make-ws1 (not (w1 ws1)) (w2 ws1) (w3 ws1) (w4 ws1)
               (w5 ws1) (w6 ws1) (w7 ws1) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]

    [(and (w2 ws1) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (not (w2 ws1)) (w3 ws1) (w4 ws1)
               (w5 ws1) (w6 ws1) (w7 ws1) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]
    [(and (not (w2 ws1)) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (not (w2 ws1)) (w3 ws1) (w4 ws1)
               (w5 ws1) (w6 ws1) (w7 ws1) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]

    [(and (w3 ws1) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (w2 ws1) (not (w3 ws1)) (w4 ws1)
               (w5 ws1) (w6 ws1) (w7 ws1) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]
    
    [(and (not (w3 ws1)) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (w2 ws1) (not (w3 ws1)) (w4 ws1)
               (w5 ws1) (w6 ws1) (w7 ws1) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]

    [(and (w4 ws1) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (w2 ws1) (w3 ws1) (not (w4 ws1))
               (w5 ws1) (w6 ws1) (w7 ws1) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]
    [(and (not (w4 ws1)) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (w2 ws1) (w3 ws1) (not (w4 ws1))
               (w5 ws1) (w6 ws1) (w7 ws1) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]

    [(and (w5 ws1) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (w2 ws1) (w3 ws1) (w4 ws1)
               (not (w5 ws1)) (w6 ws1) (w7 ws1) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]
    [(and (not (w5 ws1)) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (w2 ws1) (w3 ws1) (w4 ws1)
               (not (w5 ws1)) (w6 ws1) (w7 ws1) (w8 ws1) (wtimer ws1 (wtimer2 ws1)))]

    [(and (w6 ws1) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (w2 ws1) (w3 ws1) (w4 ws1)
               (w5 ws1) (not (w6 ws1)) (w7 ws1) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]
    [(and (not (w6 ws1)) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (w2 ws1) (w3 ws1) (w4 ws1)
               (w5 ws1) (not (w6 ws1)) (w7 ws1) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]

    [(and (w7 ws1) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (w2 ws1) (w3 ws1) (w4 ws1)
               (w5 ws1) (w6 ws1) (not (w7 ws1)) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]
    [(and (not (ws1-w7 ws1)) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (w2 ws1) (w3 ws1) (w4 ws1)
               (w5 ws1) (w6 ws1) (not (w7 ws1)) (w8 ws1) (wtimer ws1) (wtimer2 ws1))]

    [(and (w8 ws1) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (w2 ws1) (w3 ws1) (w4 ws1)
               (w5 ws1) (w6 ws1) (w7 ws1) (not (w8 ws1)) (wtimer ws1) (wtimer2 ws1))]
    [(and (not (w8 ws1)) (mouse=? mouse "button-down"))
     (make-ws1 (w1 ws1) (w2 ws1) (w3 ws1) (w4 ws1)
               (w5 ws1) (w6 ws1) (w7 ws1) (not (w8 ws1)) (wtimer ws1) (wtimer2 ws1))]
    
    [else ws1]))

(define (test ws)
  (big-bang ws
   [on-mouse mouse-test]
   [on-tick slider 1/100]
   [to-draw image1]))

(define INITIAL-STATE
  (make-ws1 #t #t #t #t #t #t #t #t 0 .47))

(test INITIAL-STATE)