;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname finalkeyhandler) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; FINAL PROJECT
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)
(require rsound/piano-tones)

; CHARSKIE
; KINLOCH's
; PART
; KEY-HANDLER


; A WorldState is a make-ws or ws that takes in the current tempo
; Where playing is defined by a boolean
; Tempo is a number between 1 and 9
; And family is a boolean where main is true and vgame is false
(define-struct ws (playing tempo family))


; KEY HANDLER PART
(define (key-handler ws key)

; PRESSING THE 1 KEY
  (cond
    [(key=? key "1")
         (make-ws (ws-playing 1 ws-family))]
    
; PRESSING THE 2 KEY
  
    [(key=? key "2")
         (make-ws (ws-playing 2 ws-family))]
   
; PRESSING THE 3 KEY
  
  [(key=? key "3")
       (make-ws (ws-playing 3 ws-family))]
   
; PRESSING THE 4 KEY
  
   [(key=? key "4")
        (make-ws (ws-playing 4 ws-family))]
    

; PRESSING THE 5 KEY
  
   [(key=? key "5")
        (make-ws (ws-playing 5 ws-family))]
    
; PRESSING THE 6 KEY
  
  [(key=? key "6")
       (make-ws (ws-playing 6 ws-family))]

; PRESSING THE 7 KEY
  
   [(key=? key "7")
        (make-ws (ws-playing 7 ws-family))]

; PRESSING THE 8 KEY
  
   [(key=? key "8")
        (make-ws (ws-playing 8 ws-family))]

; PRESSING THE 9 KEY
  
   [(key=? key "9")
        (make-ws (ws-playing 9 ws-family))]

; PRESSING THE 0 KEY
  
   [(key=? key "0")
        (make-ws (ws-playing ws-tempo (not ws-family)))]

; PRESSING THE SPACEBAR KEY

   [(key=? key " ")
        (make-ws (not ws-playing) ws-tempo ws-family)]
   [else ws]))



; Our other function tempoconv converts our input of our tempo
; of the numbers 1-9 to an actual measure of tempo to beats per minute
; Number -> Number

(define (tempoconv ws-tempo)
        (cond [(= ws-tempo 1) 12]
              [else
               (cond [(= ws-tempo 2) 24]
                          [else
                           (cond [(= ws-tempo 3) 36]
                                      [else
                                       (cond [(= ws-tempo 4) 48]
                                                  [else
                                                   (cond [(= ws-tempo 5) 60]
                                                              [else
                                                               (cond [(= ws-tempo 6) 72]
                                                                          [else
                                                                           (cond [(= ws-tempo 7) 84]
                                                                                      [else
                                                                                       (cond [(= ws-tempo 8) 96]
                                                                                                  [else
                                                                                                   (cond [(= ws-tempo 9) 108]
                                                                                                              [else ws-tempo])])])])])])])])]))


; Ok


                                                                                                                     