;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ui-update) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;FinalProject : Melody Maker

(require racket/list)
(define-struct ws [vol songpos playing? fam tempo note-list w9 timer1 timer2])

;a World State is make-ws
;where (make-ws) is (make-ws vol songpos playing? fam tempo note-list w9 timer1 timer2)

;fill in other elements of the world-state
;
;

;w9 takes in a list-of-booleans
;where a list-of booleans is one of
; '()
; (cons Boolean list-of-booleans)
; (list (list Boolean list-of-booleans))


;timer1 takes in a number from 0 to .49
;and timer2 takes in a number from .49 to .98
;--------------------------------------------------------------------------------------------------


;8x8. Each "window" will be 4 x 8 (row x column)

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
(define BLANK-WIDTH 1220)
(define BLANK-HEIGHT 250)
(define bg-color "white")

(define blank (rectangle BLANK-WIDTH BLANK-HEIGHT "solid" bg-color))
(define SLIDER (rectangle 10 232 "solid" "gray"))

;Drawn representation of 'silence'
(define dot (circle 3 "solid" "gray"))

;Exists purely for aesthetics
(define dividers (rectangle 6 BLANK-HEIGHT "solid" bg-color))
(define border (rectangle 4 BLANK-HEIGHT "solid" "black")) ;;divides the two windows
(define bot-border (rectangle BLANK-WIDTH 18 "solid" bg-color))

;--------------------------------------------------------------------------------------------------

;creates x coordinates for image placement
;Number --> Number
(define (x-coor x)
  (cond
    [(= x 1) 75]
    [else (- (* 150 x) 75)]))
(check-expect (x-coor 1) 75)
(check-expect (x-coor 5) 675)

;creates y coordinates for image placement
;Number --> Number
(define (y-coor y)
  (cond
    [(= y 1) 14.5]
    [else (- (* 29 y) 14.5)]))
(check-expect (y-coor 1) 14.5)
(check-expect (y-coor 4) 101.5)

;bg-image-dots takes in a list-of-numbers
;that is either '() or (cons Number List-of-numbers)
;List-of-numbers --> Image
(define (bg-image-dots LIST)
  (cond
    [(empty? LIST) blank]
    [else (place-image dot (x-coor 1) (y-coor (+ 1 (first LIST)))
          (place-image dot (x-coor 2) (y-coor (+ 1 (first LIST)))
          (place-image dot (x-coor 3) (y-coor (+ 1 (first LIST)))
          (place-image dot (x-coor 4) (y-coor (+ 1 (first LIST)))
          (place-image dot (x-coor 5) (y-coor (+ 1 (first LIST)))
          (place-image dot (x-coor 6) (y-coor (+ 1 (first LIST)))
          (place-image dot (x-coor 7) (y-coor (+ 1 (first LIST)))
          (place-image dot (x-coor 8) (y-coor (+ 1 (first LIST)))
                             (bg-image-dots (rest LIST))))))))))]))

(check-expect (bg-image-dots (list 0))
              (place-image dot (x-coor 1) (y-coor 1)
              (place-image dot (x-coor 2) (y-coor 1)
              (place-image dot (x-coor 3) (y-coor 1)
              (place-image dot (x-coor 4) (y-coor 1)
              (place-image dot (x-coor 5) (y-coor 1)
              (place-image dot (x-coor 6) (y-coor 1)
              (place-image dot (x-coor 7) (y-coor 1)
              (place-image dot (x-coor 8) (y-coor 1)
              blank)))))))))

;Base-image.
(define bg-image (bg-image-dots (list 0 1 2 3 4 5 6 7)))

;--------------------------------------------------------------------------------------------------

;ref is a function that takes in a
;List, n1, and n2
;where n1 is a number that corresponds with the first position in the lists
;and n2 is a number that corresponds with the second position in the lists

;Hones into into a single boolean in the WorldState List.
;List Number Number --> Boolean
(define (ref ws n1 n2)
    (list-ref (list-ref ws (sub1 n1)) (sub1 n2)))
(check-expect (ref (list (list #f #t #t #t) (list #t #f #f #f)) 2 1) #t)
(check-expect (ref (list (list #f #t #t #t) (list #t #f #f #f)) 1 1) #f)

;Rect-placer is a function that
;takes a WorldState, x, and y
;where x is a number representing the first position in the lists
;and y is a number representing the second position in the lists

;Places rectangles if a certain position in the WorldState is #t
;if it's #f, then the rectangle "disappears"
;WorldState Number Number --> Image
(define (rect-placer ws x y)
  (cond
    [(and (= x 8) (= y 9)) bg-image]
    [(= x 9) (rect-placer ws 1 (add1 y))]
    [(not (ref (ws-w9 ws) x y)) (place-image dot (x-coor x) (y-coor y)
                                             (rect-placer ws (add1 x) y))]
    [(and (= y 1)(ref (ws-w9 ws) x y)) (place-image rect1 (x-coor x) (y-coor y)
                                                    (rect-placer ws (add1 x) y))]
    
    [(and (= y 2)(ref (ws-w9 ws) x y)) (place-image rect2 (x-coor x) (y-coor y)
                                                    (rect-placer ws (add1 x) y))]

    [(and (= y 3)(ref (ws-w9 ws) x y)) (place-image rect3 (x-coor x) (y-coor y)
                                                    (rect-placer ws (add1 x) y))]

    [(and (= y 4)(ref (ws-w9 ws) x y)) (place-image rect4 (x-coor x) (y-coor y)
                                                    (rect-placer ws (add1 x) y))]

    [(and (= y 5)(ref (ws-w9 ws) x y)) (place-image rect5 (x-coor x) (y-coor y)
                                                    (rect-placer ws (add1 x) y))]
    
    [(and (= y 6)(ref (ws-w9 ws) x y)) (place-image rect6 (x-coor x) (y-coor y)
                                                    (rect-placer ws (add1 x) y))]
    
    [(and (= y 7)(ref (ws-w9 ws) x y)) (place-image rect7 (x-coor x) (y-coor y)
                                                    (rect-placer ws (add1 x) y))]
    
    [(and (= y 8)(ref (ws-w9 ws) x y)) (place-image rect8 (x-coor x) (y-coor y)
                                                    (rect-placer ws (add1 x) y))]
    [else bg-image]
    ))

;;Takes in a world-state and returns an image. Used with to-draw
;World State --> Image
(define (image1 ws)
  (place-image bot-border (/ BLANK-WIDTH 2) BLANK-HEIGHT
  (place-image SLIDER (* (ws-timer1 ws) BLANK-WIDTH) (/ 232 2)
  (place-image SLIDER (* (ws-timer2 ws) BLANK-WIDTH) (/ 232 2)
  (place-image dividers 150 (/ BLANK-HEIGHT 2)
  (place-image dividers 300 (/ BLANK-HEIGHT 2)
  (place-image dividers 450 (/ BLANK-HEIGHT 2)
  (place-image border 600 (/ BLANK-HEIGHT 2)
  (place-image dividers 600 (/ BLANK-HEIGHT 2)
  (place-image dividers 750 (/ BLANK-HEIGHT 2)
  (place-image dividers 900 (/ BLANK-HEIGHT 2)
  (place-image dividers 1050 (/ BLANK-HEIGHT 2)
  (rect-placer ws 1 1)
  ))))))))))))

;Draws the slider, includes the ability to loop. Used with on-tick
;WorldState --> WorldState
(define (slider ws)
  (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws) (ws-fam ws) (ws-tempo ws)
            (ws-note-list ws) (ws-w9 ws)
            (cond
               [(> (ws-timer1 ws) .49) 0]
               ;^makes it so the slider 'loops' once it reaches the end.
               [else   (+ TEMPO1 (ws-timer1 ws))]
               )
            ;^this is in the timer1 field of the structure
            (cond
              [(> (ws-timer2 ws) (* .49 2)) .49]
              [else   (+ TEMPO2 (ws-timer2 ws))]
              )))

(check-expect (slider (make-ws 1 2 3 4 5 6 7 .49 .60)) (make-ws 1 2 3 4 5 6 7 (+ TEMPO1 .49) (+ TEMPO2 .60)))
(check-expect (slider (make-ws 1 2 3 4 5 6 7 .60 2)) (make-ws 1 2 3 4 5 6 7 0 .49))
;The tempo of either window 1 or window 2
"REPLACE LATER WITH TEMPO FUNCTION"
;(the numbers are currently placeholders for the purpose of testing)
(define TEMPO1 1/200)
(define TEMPO2 1/500)
"How to sync these tempos with pstreams?"

;;If you ever find the slider looping out of synch, try messing
;;with the 0.49. (I don't know why .49 is the way it is, but
;;trial and error led me to it.)

;--------------------------------------------------------------------------------------------------

;LC (list-checker) is a function that
;takes in a list-of-booleans, n1, and n2
;where n1 is a number that represents the first position in the list
;and n2 is a number that represents the second position in the list

;Takes the boolean specified by the two numbers and make it either #t--> #f or #f --> #t
;List Number Number --> List
(define (LC ws n1 n2)
  (list-set ws (sub1 n1) (list-set (list-ref ws (sub1 n1)) (sub1 n2) (not (list-ref (list-ref ws (sub1 n1))(sub1 n2))))))
  
;;;;It will be used within mouse events so we could change the states whenever a mouse-event happens
;n1 and n2 are subtracted by one because list position start with 0, but it's easier for me to think it starts at 1.

(check-expect (LC (list (list #f #t #f #t) (list #t #t #t #t)) 1 2) (list (list #f #f #f #t) (list #t #t #t #t)))
(check-expect (LC (list (list #f #t #f #t) (list #t #t #t #t)) 2 4) (list (list #f #t #f #t) (list #t #t #t #f)))

;;abstraction for x-LC and y-LC
;Number Number Number --> Number
(define (LC-xy var n1 n2)
  (cond
    [(<= var n1) 1]
    [(< n1 var n2) (ceiling (/ var n1))]
    [else 9]))

;x-LC deals with the x-coordinates of the image
;Number --> Number
(define (x-LC x)
  (LC-xy x 150 1200))
(check-expect (x-LC 200) 2)
(check-expect (x-LC 1500) 9)
(check-expect (x-LC 129.0939) 1)

;y-LC deals with the y-coordinates of the image
;Number --> Number
(define (y-LC y)
  (LC-xy y 29 232))
(check-expect (y-LC 3) 1)
(check-expect (y-LC 70) 3)
(check-expect (y-LC 500) 9)
;--------------------------------------------------------------------------------------------------

;The function mouse-test will handle mouse events
;It will change dots/rectangles to rectangles/dots
;according to whether the user clicked on it or not.
;Otherwise, the world stays the same.
;WorldState Number(x-coor) Number(y-coor) MouseEvent --> World
(define (mouse-test ws x y key)
    (cond
      [(mouse=? "button-down" key)
       (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws) (ws-fam ws) (ws-tempo ws)
                (ws-note-list ws) (LC (ws-w9 ws) (x-LC x) (y-LC y)) (ws-timer1 ws) (ws-timer2 ws))]
      [else ws]
      ))

;--------------------------------------------------------------------------------------------------
(define (test ws)
  (big-bang ws
    [on-mouse mouse-test]
    [on-tick slider 1/100]
    [to-draw image1]))
;--------------------------------------------------------------------------------------------------
(define w9-Initial (list (list #t #f #f #f #f #f #f #f #f) (list #f #t #f #f #f #f #f #f #f) (list #f #f #t #f #f #f #f #f #f) (list #f #f #f #t #f #f #f #f #f)
                         (list #f #f #f #f #t #f #f #f #f) (list #f #f #f #f #f #t #f #f #f) (list #f #f #f #f #f #f #t #f #f) (list #f #f #f #f #f #f #f #t #f)
                         (list #f #f #f #f #f #f #f #f #f)))

(define INITIAL-STATE
  (make-ws 1 2 3 4 5 6 w9-Initial 0 .49))

(test INITIAL-STATE)





