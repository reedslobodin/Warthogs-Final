;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ui-update) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;FinalProject : Melody Maker

(require racket/list)
(require rsound)
(define-struct ws [vol songpos playing? fam tempo note-list w9 timer1 timer2])

; A WorldState is a make-ws or ws that will determine the state of our World MelodyMaker.
; Our WorldState will be defined by a structure with many parts,

; vol determines the volume of the current stream,

; songpos determines the position we are at in the stream of sounds

; playing? is a boolean where #t means it is playing and #f means it is paused

; fam is a structure including a name determined by a string and number that determines
; the family of notes we are using for the MelodyMaker at the moment, and the number within
; that family. The name is either one of main or vgame and numbers can vary.
; Our list of family numbers is going to be: main 50 , vgame 50 , main 25 , vgame 25
#;(define-struct famstruct (main50 vgame50 main25 vgame25))

;wlist takes in a list-of-booleans
;where a list-of booleans is one of
; '()
; (cons Boolean list-of-booleans)
; (list (list Boolean list-of-booleans))
; It will be a list of booleans, which will tell us which rectangles are on/off at the moment,
; #t means the rectangle is showing. #f means it is not.

; tempo determines the cuurent tempo of the stream on a scale of 1-9

; and note-list is the list of midi-note-numbers that the function will run through 

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
(define BLANK-HEIGHT 400)
(define DIVIDER-HEIGHT 250)
(define bg-color "white")

(define blank (rectangle BLANK-WIDTH BLANK-HEIGHT "solid" bg-color))
(define SLIDER (rectangle 10 232 "solid" "gray"))
(define DIRECTIONS (text "Try pressing the keys...\n - 1 through 9 to change the volume\n - Spacebar to mute or unmute\n - F to change the category of sounds\n
(The changes might take some time)" 15 "black"))

;Drawn representation of 'silence'
(define dot (circle 3 "solid" "gray"))

;Exists purely for aesthetics
(define dividers (rectangle 6 BLANK-HEIGHT "solid" bg-color))
(define border (rectangle 4 DIVIDER-HEIGHT "solid" "black")) ;;divides the two windows
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
(check-expect (ref (list (list #f #f #t #t) (list #t #f #f #f)) 2 1) #t)
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
  (place-image DIRECTIONS (/ BLANK-WIDTH 2) 300
  (place-image bot-border (/ BLANK-WIDTH 2) 250
  (place-image SLIDER (* (ws-timer1 ws) BLANK-WIDTH) (/ 232 2)
  (place-image SLIDER (* (ws-timer2 ws) BLANK-WIDTH) (/ 232 2)
  (place-image dividers 150 (/ BLANK-HEIGHT 2)
  (place-image dividers 300 (/ BLANK-HEIGHT 2)
  (place-image dividers 450 (/ BLANK-HEIGHT 2)
  (place-image border 600 (/ DIVIDER-HEIGHT 2)
  (place-image dividers 600 (/ BLANK-HEIGHT 2)
  (place-image dividers 750 (/ BLANK-HEIGHT 2)
  (place-image dividers 900 (/ BLANK-HEIGHT 2)
  (place-image dividers 1050 (/ BLANK-HEIGHT 2)
  
  (rect-placer ws 1 1)
  )))))))))))))


;;taken from sleepy-dj
;; how long should each queued segment be, in seconds?
(define PLAY-SECONDS 1/20)
;;taken from sleepy-dj
;; the longest lead time for which we'll queue the next sound
(define (MAX-QUEUE-INTERVAL nl) (* 4 nl))


(define pstream1 (make-pstream))
(define pstream2 (make-pstream))

;;taken from sleepy-dj
;; .. in frames?
(define PLAY-FRAMES (* PLAY-SECONDS FRAME-RATE))
;determines whether to queue a new section if too close to the end
;frame --> boolean
;(define (end-frames song-frame)
  ;(cond
   ; [(< (+ song-frame PLAY-FRAMES) (rs-frames Ia)) (+ song-frame PLAY-FRAMES)]
    ;R[else (- (rs-frames Ia) 1)]))
;; taken from sleepy-dj and then modified to queue 8 pstreams
;; queue up the next fragment
;(define (queue-notes song-frame frame-to-play val)
  ;(cond
   ; [(< song-frame (- (rs-frames Ia) PLAY-FRAMES))
 ; (andqueue 1a (clip Ia song-frame (end-frames song-frame))frame-to-play val)]
  ;  [else val]))
(define note-length
  15000 )



;;cheesy bit of code to do two things with one function
;;taken from sleep-dj
(define (both a b) b)

;what to do on every tick
(define (ticker ws)
  (both (set-volume ws)
  (cond [(ws-playing? ws)
(big-queue (ws-w9 ws) ws 0)]
[else ws]
        )))

;queues all of the notes in the worldstate list      
(define (big-queue lol ws counter)
(cond [(empty? lol) ws]
      [else (cond [(< counter 4)
(both (note-queuer ws (bools-to-notes 0 (first lol)) counter pstream1 note-length 1) (big-queue (rest lol) ws (add1 counter)))]
                
                  [else (both (note-queuer ws (bools-to-notes 0 (first lol)) (- counter 4) pstream2 note-length 2) (big-queue (rest lol) ws (add1 counter)))])]))





;returns list of midi note numbers based off of a list of booleans
;list of booleans --> list of numbers
   (define (bools-to-notes counter lob)
     (cond [(empty? lob) '()]
           [else (cond [(not (first lob)) (bools-to-notes (add1 counter) (rest lob))]
                 [else (cons (bool-translator counter) (bools-to-notes (add1 counter) (rest lob)))])]))

(check-expect (bools-to-notes 0 (list #t #f #f #f #f #f #f #f #f)) '(72))
(check-expect (bools-to-notes 0 (list #t #f #f #f #f #t #f #f #f)) '(72 64))


;returns a midi note number based off of the location of a boolean in a list of booleans
;number --> number  
  (define (bool-translator counter)
  (cond [(= counter 0) 72]
        [(= counter 1) 71]
        [(= counter 2) 69]
        [(= counter 3) 67]
        [(= counter 4) 65]
        [(= counter 5) 64]
        [(= counter 6) 62]
        [(= counter 7) 60]
        [(= counter 8) 59]))

;queues notes based on a list of notes and a note position
;list-of-notes --> ws
 (define (note-queuer ws lon note-place pstream nl num)
   (cond [(empty? lon) ws]
         [else (andqueue pstream (note-maker ws (first lon) nl) 
                         (cond [(= num 1)
                                (+(+ (pstream-current-frame pstream) (* 4 nl)) (* note-place nl))]
                               [else (+(+ (pstream-current-frame pstream) (* 4 nl)) (* note-place nl))])
                         (note-queuer ws (rest lon) note-place pstream nl num))])) 
   ;decides which type of rsound to make based on the current family value
   ; ws midi number --> rsound
   (define (note-maker ws midi nl)
     (cond [(= (ws-fam ws) 1) (synth-note "vgame" 50 midi nl)]
           [(= (ws-fam ws) 2) (synth-note "main" 50 midi nl)]
           [(= (ws-fam ws) 3) (synth-note "vgame" 25 midi nl)]
           [(= (ws-fam ws) 4) (synth-note "main" 25 midi nl)]))
   

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
;sets volumes of both pstreams based on the worldstate volume value
(define (set-volume ws)
  (both (pstream-set-volume! pstream1 (ws-vol ws))
        (pstream-set-volume! pstream2 (ws-vol ws))))

;rotates through the different family values
(define (togglefam ws)
  (cond [(= (ws-fam ws) 1) 2]
        [(= (ws-fam ws) 2) 3]
        [(= (ws-fam ws) 3) 4]
        [(= (ws-fam ws) 4) 1]))


(define (key-handler ws key)
; PRESSING THE 1 KEY
  (cond
    [(key=? key "1")
         (make-ws .1 (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                   (ws-tempo ws)
                    (ws-note-list ws) (ws-w9 ws) (ws-timer1 ws) (ws-timer2 ws))]
    
; PRESSING THE 2 KEY
  
    [(key=? key "2")
         (make-ws .2 (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    (ws-tempo ws)
                    (ws-note-list ws) (ws-w9 ws) (ws-timer1 ws) (ws-timer2 ws))]

; PRESSING THE 3 KEY
  
    [(key=? key "3")
         (make-ws .3 (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    (ws-tempo ws)
                    (ws-note-list ws) (ws-w9 ws) (ws-timer1 ws) (ws-timer2 ws))]

; PRESSING THE 4 KEY
  
    [(key=? key "4")
         (make-ws .4 (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    (ws-tempo ws)
                    (ws-note-list ws) (ws-w9 ws) (ws-timer1 ws) (ws-timer2 ws))]
    
; PRESSING THE 5 KEY
  
    [(key=? key "5")
         (make-ws .5 (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    (ws-tempo ws)
                    (ws-note-list ws) (ws-w9 ws) (ws-timer1 ws) (ws-timer2 ws))]

; PRESSING THE 6 KEY
  
    [(key=? key "6")
         (make-ws .6 (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    (ws-tempo ws)
                    (ws-note-list ws) (ws-w9 ws) (ws-timer1 ws) (ws-timer2 ws))]

; PRESSING THE 7 KEY
  
    [(key=? key "7")
         (make-ws .7 (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    (ws-tempo ws)
                    (ws-note-list ws) (ws-w9 ws) (ws-timer1 ws) (ws-timer2 ws))]

; PRESSING THE 8 KEY
  
    [(key=? key "8")
         (make-ws .8 (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                   (ws-tempo ws)
                    (ws-note-list ws) (ws-w9 ws) (ws-timer1 ws) (ws-timer2 ws))]

; PRESSING THE 9 KEY
  
    [(key=? key "9")
         (make-ws .9 (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    (ws-tempo ws)
                    (ws-note-list ws) (ws-w9 ws) (ws-timer1 ws) (ws-timer2 ws))]
    ;presssing the f key
    [(key=? key "f")
            (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws) (togglefam ws)
                    (ws-tempo ws)
                    (ws-note-list ws) (ws-w9 ws) (ws-timer1 ws) (ws-timer2 ws))]
    
          



;PRESSING SPACEBAR
[(key=? key " ")
 (cond[(ws-playing? ws)
(make-ws (ws-vol ws) (ws-songpos ws) #f (ws-fam ws) (ws-tempo ws)         
(ws-note-list ws) (ws-w9 ws) (ws-timer1 ws) (ws-timer2 ws))]
      [else (make-ws (ws-vol ws) (ws-songpos ws) #t (ws-fam ws) (ws-tempo ws)
(ws-note-list ws) (ws-w9 ws) (ws-timer1 ws) (ws-timer2 ws))]
      )]
[else ws]))




(define (test ws)
  (big-bang ws
    [on-mouse mouse-test]
    [on-tick ticker (/ (* 4 note-length) FRAME-RATE)]
    [to-draw image1]
    [on-key key-handler]))
;--------------------------------------------------------------------------------------------------
(define w9-Initial (list (list #t #f #f #f #f #f #f #f #f) (list #f #t #f #f #f #f #f #f #f) (list #f #f #t #f #f #f #f #f #f) (list #f #f #f #t #f #f #f #f #f)
                         (list #f #f #f #f #t #f #f #f #f) (list #f #f #f #f #f #t #f #f #f) (list #f #f #f #f #f #f #t #f #f) (list #f #f #f #f #f #f #f #t #f)
                         (list #f #f #f #f #f #f #f #f #f)))

(define INITIAL-STATE
  (make-ws 1 2 #t 1 .5 6 w9-Initial 0 0))

(test INITIAL-STATE)





