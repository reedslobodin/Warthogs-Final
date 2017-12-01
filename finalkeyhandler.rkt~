;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname finalkeyhandler) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; FINAL PROJECT
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)
(require rsound/piano-tones)


; DEFINITION OF THE WORLDSTATE BY CHARSKIE KINLOCH

; A WorldState is a make-ws or ws that will determine the state of our World MelodyMaker.
; Our WorldState will be defined by a structure with many parts,

; vol determines the volume of the current stream,

; songpos determines the position we are at in the stream of sounds

; playing? is a boolean where #t means it is playing and #f means it is paused

; fam is a structure including a name determined by a string and number that determines
; the family of notes we are using for the MelodyMaker at the moment, and the number within
; that family. The name is either one of main or vgame and numbers can vary.
; Our list of family numbers is going to be: main 50 , vgame 50 , main 25 , vgame 25
(define-struct famstruct (main50 vgame50 main25 vgame25))


; tempo determines the cuurent tempo of the stream on a scale of 1-9

; and note-list is the list of midi-note-numbers that the function will run through 

; wlist is a list of 8 booleans that tells us which rectangles are on/off at the moment,
; a #t representing the rectangle is showing and a #f representing the rectangle for that
; column is not showing. The booleans will be in order for columns fleft to right, columns 1-8.

; timer1 is used to help represent a position for the slider on the first group of four notes.

; timer2 is used to help represent a position for the 2nd slider for the second group of four notes.

(define-struct ws (vol songpos playing? fam tempo note-list
                   wlist timer1 timer2))

; CHARSKIE
; KINLOCH's
; PART
; KEY-HANDLER

; KEY HANDLER PART

(define familylist (list "main" "vgame" "path"))

(define (key-handler ws key)

; (define (temposhort num)
;  [(key=? key "num")
;       (make-ws (ws-vol ws-songpos ws-playing? ws-fam num ws-note-list ws-wlist ws-timer1 ws-timer2))]

; PRESSING THE 1 KEY
  (cond
    [(key=? key "1")
         (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    1
                    (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws))]

; (temposhort 1)
    
; PRESSING THE 2 KEY
  
    [(key=? key "2")
         (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    2
                    (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws))]

; (temposhort 2)
    
; PRESSING THE 3 KEY
  
    [(key=? key "3")
         (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    3
                    (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws))]

; (temposhort 3)
   
; PRESSING THE 4 KEY
  
    [(key=? key "4")
         (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    4
                    (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws))]
    
; (temposhort 4)
   
; PRESSING THE 5 KEY
  
    [(key=? key "5")
         (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    5
                    (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws))]

; (temposhort 5)
   
; PRESSING THE 6 KEY
  
    [(key=? key "6")
         (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    6
                    (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws))]

; (temposhort 6)
  
; PRESSING THE 7 KEY
  
    [(key=? key "7")
         (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    7
                    (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws))]

; (temposhort 7)
   
; PRESSING THE 8 KEY
  
    [(key=? key "8")
         (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    8
                    (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws))]

; (temposhort 8)
   
; PRESSING THE 9 KEY
  
    [(key=? key "9")
         (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws) (ws-fam ws)
                    9
                    (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws))]
; (temposhort 9)
   
; PRESSING THE 0 KEY
  
   [(key=? key "0")
        (make-ws (ws-vol ws) (ws-songpos ws) (ws-playing? ws)
                   (togglefam (ws-fam ws))
                   (ws-tempo ws) (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws))]

; PRESSING THE SPACEBAR KEY

   [(key=? key " ")
        (make-ws (ws-vol ws) (ws-songpos ws)
                   (not ws-playing?)
                   (ws-fam ws) (ws-tempo ws) (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws))]


   [else ws]))



; Our other function tempoconv takes in a WorldState and determines what the tempo is
; and then converts this tempo represented by converts our input of our tempo represented by
; the numbers 1-9 to an actual measure of tempo to beats per minute.
; Number -> Number

(define (tempoconv ws)
        (cond
          [(= (ws-tempo ws) 1) 12]
          [else (cond
                  [(= (ws-tempo ws) 2) 24]
                  [else (cond
                          [(= (ws-tempo ws) 3) 36]
                          [else (cond
                                  [(= (ws-tempo ws) 4) 48]
                                  [else (cond
                                          [(= (ws-tempo ws) 5) 60]
                                          [else (cond
                                                  [(= (ws-tempo ws) 6) 72]
                                                  [else (cond
                                                          [(= (ws-tempo ws) 7) 84]
                                                          [else (cond
                                                                  [(= (ws-tempo ws) 8) 96]
                                                                  [else (cond
                                                                          [(= (ws-tempo ws) 9) 108]
                                                                          [else (ws-tempo ws)])])])])])])])])]))


; Ok
; Here is where we want to take the number the tempoconv produces and use it to change the tempo of
; the sounds in the pstream. So we will need to call this on each sound in the pstream.
; Number -> Sound
; (resample/interp (tempoconv ws-tempo) SOUND)??????????????


; Our function tempoconv2 is designed specifically for the sliders and takes in a ws and from it produces
; a fraction representing the speed the slider will be at.
; WorldState -> Number
(define (tempoconv2 ws)
   (cond
     [(= (ws-tempo ws) 1) 1/900]
     [else (cond
             [(= (ws-tempo ws) 2) 1/800]
             [else (cond
                     [(= (ws-tempo ws) 3) 1/700]
                     [else (cond
                             [(= (ws-tempo ws) 4) 1/600]
                             [else (cond
                                     [(= (ws-tempo ws) 5) 1/500]
                                     [else (cond
                                             [(= (ws-tempo ws) 6) 1/400]
                                             [else (cond
                                                     [(= (ws-tempo ws) 7) 1/300]
                                                     [else (cond
                                                             [(= (ws-tempo ws) 8) 1/200]
                                                             [else (cond
                                                                     [(= (ws-tempo ws) 9) 1/100]
                                                                     [else (ws-tempo ws)])])])])])])])])]))

; HOW DO WE MAKE SURE THE SLIDER SPEED AND SOUND SPEED ALIGN???
; Catherine Says Guess and check once it is all over

; (define-struct ws (vol songpos playing? fam tempo note-list
;                    wlist timer1 timer2))



; OUR FUNCTION (togglefam changes between the elements of our family of notes list and
; when one is an input it produces the next.
; WorldState -> WorldState
(define (togglefam ws)
  (cond [(equal? (ws-fam ws) main25)
         (make-ws ( (ws-vol ws) (ws-songpos ws) (ws-playing? ws)
                     vgame25
                    (ws-tempo ws) (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws)))]
        [else (cond [(equal? (ws-fam ws) vgame25)
                     (make-ws ( (ws-vol ws) (ws-songpos ws) (ws-playing? ws)
                                 main50
                                (ws-tempo ws) (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws)))]
                    [else (cond [(equal? (ws-fam ws) main50)
                                 (make-ws ( (ws-vol ws) (ws-songpos ws) (ws-playing? ws)
                                             vgame50
                                            (ws-tempo ws) (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws)))]
                                [else (cond [(equal? (ws-fam ws) vgame50)
                                             (make-ws ( (ws-vol ws) (ws-songpos ws) (ws-playing? ws)
                                                         main25
                                                        (ws-tempo ws) (ws-note-list ws) (ws-wlist ws) (ws-timer1 ws) (ws-timer2 ws)))]
                                            [else ws])])])]))

; Our next function (famplay takes in a worldstate and then reads the family part of the
; worldstate and determines what note to play based on the family designated.
; WorldState -> Synth-Note
(define (famplay ws)
  (cond [(equal? (ws-fam ws) main25) (synth-note "main" 25 60 (* (tempoconv (ws-tempo ws)) 500))]
        [else (cond [(equal? (ws-fam ws) main50) (synth-note "main" 50 60 (* (tempoconv (ws-tempo ws)) 500))]
                    [else (cond [(equal? (ws-fam ws) vgame25) (synth-note "vgame" 25 60 (* (tempoconv (ws-tempo ws)) 500))]
                                [else (cond [(equal? (ws-fam ws) vgame50) (synth-note "vgame" 50 60 (* (tempoconv (ws-tempo ws)) 500))]
                                            [else ws])])])]))   
                                     




                                                                                                                     