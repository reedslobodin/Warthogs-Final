;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Warthogs-Final) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)
(require racket/string)
(require 2htdp/universe)
(require rsound)

;here  lies the final work of the legendary WARTHOGS
;none may gaze upon this code without proper mental preparation beforehand
;beware mortals, for what your eyes see may overwhelm your mind
;now enter and experience a world like no other



;A ws is
;(make-ws vol songpos playing? family tempo note-list)
;where volume is a number between zero and one
;songpos is a number between 1 and zero
;playing? is a boolean
;tempo is a whole number between 1 and 9
;family is a family of notes for synth-note
;note-list is a list of midi note numbers
(define-struct ws (vol songpos playing? fam tempo note-list))

(define grid1 (* screen-width 1/8))
(define grid2 (* screen-width 1/4))
(define grid3 (* screen-width 3/8))
(define grid4 (* screen-width 1/2))
(define grid5 (* screen-width 5/8))
(define grid6 (* screen-width 3/4))
(define grid7 (* screen-width 7/8))
(define grid8 screen-width)

(define screen-width 400)

(define TICK-LEN 1/40)
(define (play-note midi)6)

  

(define (ticker ws)
ws
  )



(define (keyhandler ws key)
ws
  )


(define (mousehandler ws x y event)
(cond [(string=? event "button-down")
       

  )
(define (clicker-finder x y)
  (cond [(< 0 x grid1)]
        [(< grid1 x grid2)]
        [(< grid2 x grid3)]
        [(< grid3 x grid4)]
        [(< grid4 x grid5)]
        [(< grid5 x grid6)]
        [(< grid6 x grid7)]
        [(< grid7 x grid8)]
        
        
  

 (define (draw-image ws)

(square 20 "solid" "blue")

   )
  




(define initial-state (make-ws .5 0 #f 5 "main"  '()))



(big-bang initial-state
          [on-tick ticker TICK-LEN]
          [on-key keyhandler]
          [on-mouse mousehandler]
          [to-draw draw-image]
          )
           
