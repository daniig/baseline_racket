#lang racket
(require 2htdp/image)
(require 2htdp/universe)
#;(require "controles.rkt")

; * You can try choosing “No debugging or profiling” in the DrRacket language settings
; of “Determine language from source”, but that will have a negligible effect for this code
; which spends essentially all its time in library graphics code.

; * You can try running outside of DrRacket, with the comman-line racket which is in your
;  racket installation's bin/ directory/folder.

; * To time a block wrap it with  time  ...
(time (define l (make-list (expt 10 6) #true))
      (length l ))
;  ... which prints the time taken while still producing the result of the last expression.

(struct State
  (angle)
  #:transparent)

(struct pos-ang
  (ro
   a)   ; alpha
  #:transparent)

(define FPS 60)
(define GAME-WIDTH 400)
(define GAME-HEIGHT GAME-WIDTH)
(define ROT-SPEED (/ pi 2)) ; radians per second

(define INITIAL-STATE
  (State 0))

(define (update-state s)
  (State (update-angle (State-angle s))))

(define (update-angle a)
  (define new-angle (+ a (/ ROT-SPEED FPS)))
  (if (> new-angle (* 2 pi))
      (- new-angle (* 2 pi))
      new-angle))

(define g-cactus (bitmap "cactus1.png"))

(define CACTI-COUNT 30)
(define CACTI-LIST (for/list ([i (in-range CACTI-COUNT)])
                     (pos-ang (* 200 (random))
                              (* i (/ (* 2 pi) CACTI-COUNT)))))

; * 2htp/image is a structured graphics library, so the following create a datastructure
;  representing commands to do the drawing, and are very fast.

(define (draw-object graphic p bg) ; p: angular position, bg: background image
  (place-image/align
   graphic
   (+ (* (pos-ang-ro p) (cos (pos-ang-a p))) (/ GAME-WIDTH 2))
   (+ (* (pos-ang-ro p) (sin (pos-ang-a p))) (/ GAME-HEIGHT 2))
   "center" "bottom"
   bg))

(define (draw-cacti bg angle)
  (define (draw-one-cactus p bg)
    (draw-object g-cactus (pos-ang (pos-ang-ro p) (+ (pos-ang-a p) angle)) bg))
  (foldl draw-one-cactus bg CACTI-LIST))

(define (draw-state s)
  ; * 2htdp/image is stateless, so the backrgound could be created once outside the function,
  ;  but again it's just a tiny datastructure so that's negligible.
  (define blank-scene (empty-scene GAME-WIDTH GAME-HEIGHT))
  ; * This shows that creating the structured graphic is instantaneous ...
  (define structured-graphic (time (draw-cacti blank-scene (State-angle s))))
  ;  ... but we can also ask to render it as pixels in a bitmap before giving it to big-bang, by
  ;  calling  freeze  , which is essentially doing some of what big-bang would do if we didn't ...
  (time (freeze structured-graphic)))

(provide update-state
         draw-state
         INITIAL-STATE
         State
         FPS)
