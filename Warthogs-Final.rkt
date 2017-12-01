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

(define screen-width 400)
(define screen-height 400)

(define grid1 (* screen-width 1/8))
(define grid2 (* screen-width 1/4))
(define grid3 (* screen-width 3/8))
(define grid4 (* screen-width 1/2))
(define grid5 (* screen-width 5/8))
(define grid6 (* screen-width 3/4))
(define grid7 (* screen-width 7/8))
(define grid8 screen-width)


(define height1 (* screen-height 1/8))
(define height2 (* screen-height 1/4))
(define height3 (* screen-height 3/8))
(define height4 (* screen-height 1/2))
(define height5 (* screen-height 5/8))
(define height6 (* screen-height 3/4))
(define height7 (* screen-height 7/8))
(define height8 screen-height)


(define midi1 60)
(define midi2 62)
(define midi3 64)
(define midi4 65)
(define midi5 67)
(define midi6 69)
(define midi7 71)
(define midi8 72)





(define TICK-LEN 1/40)
(define (play-note midi)6)

  

(define (ticker ws)
ws
  )



(define (keyhandler ws key)
ws
  )


(define (mousehandler ws x y event)

(cond [(string=? event "button-down") (note-changer ws (clicker-length x) (clicker-height y))]
      [else ws])
       

  )
;returns a number between 1 and 8 to determine which horizontal section the click is in
;Number --> Number
(define (clicker-length x)
  (cond [(< 0 x grid1) 1]
        [(< grid1 x grid2) 2]
        [(< grid2 x grid3) 3]
        [(< grid3 x grid4) 4]
        [(< grid4 x grid5) 5]
        [(< grid5 x grid6) 6]
        [(< grid6 x grid7) 7]
        [(< grid7 x grid8) 8]))
;returns a number between 1 and 8 to determine which vertical section the click is in
;Number --> Number    
(define (clicker-height y)
  (cond[(< 0 y height1) 1]
       [(< height1 y height2) 2]
       [(< height2 y height3) 3]
       [(< height3 y height4) 4]
       [(< height4 y height5) 5]
       [(< height5 y height6) 6]
       [(< height6 y height7) 7]
       [(< height7 y height8) 8]))

;determines what midi note number each vertical section should correspond to
;Number --> Number
(define (note-checker height)
  (cond[(= height 1) midi1]
       [(= height 2) midi2]
       [(= height 3) midi3]
       [(= height 4) midi4]
       [(= height 5) midi5]
       [(= height 6) midi6]
       [(= height 7) midi7]
       [(= height 8) midi8]))

;returns the list of midi note numbers that change based on a click
;WorldState Number Number --> List-of-Numbers
(define (note-changer ws grid height)
  (cond [(= grid 0)
         (cond
           [(= (note-checker height) (first ws-note-list)) (cons 0 (rest ws-note-list))];turn note off
           [else (cons (note-checker height) (rest ws-note-list))])];turn note on
        [else (cons(first ws-note-list) (note-changer (rest ws-note-list) (sub1 grid)))]))
(cond [(string=? event "button-down")])
       

  
(define (clicker-finder x y)
  (cond [(< 0 x grid1)]
        [(< grid1 x grid2)]
        [(< grid2 x grid3)]
        [(< grid3 x grid4)]
        [(< grid4 x grid5)]
        [(< grid5 x grid6)]
        [(< grid6 x grid7)]
        [(< grid7 x grid8)]))
        
        
  



  
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
           
