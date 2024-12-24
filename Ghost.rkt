; Constants:
; WIDTH and HEIGHT are Numbers
; GHOSTPIC is an Image
; INITIALGHOSTAMOUNT is a Number and stands for the amount of ghosts
; GHOSTSPEED-X is Number and represents the speed at which ghosts move along the x-axis each tick
; GHOSTSPEED-Y is Number and represents the speed at which ghosts move along the y-axis each tick

(require 2htdp/image)

(define WIDTH 1000)
(define HEIGHT 600)
(define GHOSTPIC (bitmap "https://github.com/aeroplane12/Ghost/main/ghost.png"))
(define INITIALGHOSTAMOUNT 3)
(define GHOSTMAXSPEED-X 10)
(define GHOSTMAXSPEED-Y 10)

(define-struct Ghost (x y image))
;Ghost is a structure: (make-Ghost number number image)
;interp. represents the funny little guys just floating around

(define-struct WorldState (ghosts score))
;WorldState is a structure: (make-WorldState List-of Ghost number)
;interp. contains a list of all (un-)living ghosts and your current kill-count

;hit-ghost?: Ghost number number -> boolean
;Checks if given number coordinates are in a given ghost
(check-expect (hit-ghost? (make-Ghost 5 5 GHOSTPIC) 0 (+ (/ (image-width GHOSTPIC) 2) 6)) #false)
(check-expect (hit-ghost? (make-Ghost 5 5 GHOSTPIC) 5 5) #true)
(check-expect (hit-ghost? (make-Ghost 5 5 GHOSTPIC) 4 4) #true)
(define (hit-ghost? g m-x m-y) (cond
                                 [(and (and (<= (-(Ghost-x g) (/ (image-width(Ghost-image g)) 2)) m-x) (>= (+(Ghost-x g) (/ (image-width(Ghost-image g)) 2)) m-x)) (and (<= (-(Ghost-y g) (/(image-height (Ghost-image g))2)) m-y) (>= (+ (Ghost-y g) (/ (image-height(Ghost-image g)) 2)) m-y))) #true]
                                 [else #false]))

;hit?: List-of Ghost number number -> boolean
;true if it hits, false if it doesn't
(check-expect (hit? (make-list 10 (make-Ghost 5 5 GHOSTPIC)) 0 (+ (/ (image-width GHOSTPIC) 2) 6)) #false)
(check-expect (hit? (list (make-Ghost 3000 3000 GHOSTPIC) (make-Ghost 5 5 GHOSTPIC)) 6 6) #true)
(define (hit? lg m-x m-y) (cond
                            [(empty? lg)#false]
                            [(hit-ghost? (first lg) m-x m-y) #true]
                            [else (hit? (rest lg) m-x m-y)]))

;remove-ghost: List-of Ghost number number -> List-of Ghost
;removes the first ghost that was hit
(check-expect (remove-ghost (make-list 10 (make-Ghost 5 5 GHOSTPIC))  0 (+ (/ (image-width GHOSTPIC) 2) 6)) (make-list 10 (make-Ghost 5 5 GHOSTPIC)))
(check-expect (remove-ghost (list (make-Ghost 3000 3000 GHOSTPIC) (make-Ghost 5 5 GHOSTPIC)) 5 5)(list (make-Ghost 3000 3000 GHOSTPIC)))
(define (remove-ghost lg m-x m-y) (cond
                   [(empty? lg) empty]
                   [(hit-ghost? (first lg) m-x m-y)(rest lg)]
                   [else (cons (first lg) (remove-ghost (rest lg) m-x m-y))]))

;mouse-handler: WorldState number number String -> WorldState
;checks whether you hit the right button and the ghost
(check-expect (mouse-handler (make-WorldState (make-list 10 (make-Ghost 0 0 GHOSTPIC))0) (+ (/ (image-width GHOSTPIC) 2) 1)  0 "button-down") (make-WorldState (make-list 10 (make-Ghost 0 0 GHOSTPIC)) 0))
(check-expect (mouse-handler (make-WorldState (list (make-Ghost 3000 3000 GHOSTPIC) (make-Ghost 5 5 GHOSTPIC)) 2) 0 0 "button-down") (make-WorldState (list (make-Ghost 3000 3000 GHOSTPIC)) 3))
(check-expect (mouse-handler (make-WorldState (list (make-Ghost 3000 3000 GHOSTPIC) (make-Ghost 5 5 GHOSTPIC)) 2) 0 0 "wheel-up") (make-WorldState (list (make-Ghost 3000 3000 GHOSTPIC)(make-Ghost 5 5 GHOSTPIC)) 2))
(define (mouse-handler ws m-x m-y m-event)(cond
                                            [(string=? m-event "button-down")(cond
                                                                                    [(hit? (WorldState-ghosts ws) m-x m-y)(make-WorldState (remove-ghost (WorldState-ghosts ws) m-x m-y) (+ (WorldState-score ws) 1))]
                                                                                    [else ws])]
                                            [else ws]))

;render: WorldState -> image
;displays all Ghosts and Score in given WorldState
(check-expect (image? (render (make-WorldState (list (make-Ghost 3000 3000 GHOSTPIC) (make-Ghost 5 5 GHOSTPIC)) 2))) #true)
(define (render ws) (render-ghost (WorldState-ghosts ws)(place-image (text(string-append "Score: " (number->string (WorldState-score ws))) 20 "black") (/ WIDTH 2) 20 (empty-scene WIDTH HEIGHT))))

;render-ghost: List-of Ghost image -> image
;displays all Ghosts in given List
(define (render-ghost lg img) (cond
                                  [(empty? lg) img]
                                  [(cons? lg)(render-ghost (rest lg) (place-image (Ghost-image (first lg)) (Ghost-x(first lg)) (Ghost-y(first lg)) img))]))

;tick: WorldState -> WorldState
;moves the Ghosts around
(check-expect (equal? (tick (make-WorldState (list (make-Ghost 3000 3000 GHOSTPIC) (make-Ghost 5 5 GHOSTPIC)) 2)) (make-WorldState (list (make-Ghost (modulo (+ 3000 GHOSTMAXSPEED-X) WIDTH) (modulo (+ 3000 GHOSTMAXSPEED-Y) HEIGHT) GHOSTPIC) (make-Ghost (modulo(+ 5 GHOSTMAXSPEED-X) WIDTH) (modulo (+ 5 GHOSTMAXSPEED-Y) HEIGHT) GHOSTPIC)) 2)) #true)
(define (tick ws) (make-WorldState (tick-ghost (WorldState-ghosts ws)) (WorldState-score ws)))

;tick-ghost: List-of Ghost -> List-of Ghost
;moves a ghost at a time
(define (tick-ghost lg) (cond
                   [(empty? lg) empty]
                   [else (cons (make-Ghost (modulo (+ (Ghost-x (first lg)) GHOSTMAXSPEED-X) WIDTH) (modulo (+ (Ghost-y (first lg)) GHOSTMAXSPEED-Y) HEIGHT) (Ghost-image (first lg))) (tick-ghost (rest lg)))]))

;end-of-the-world: WorldState -> boolean
;checks whether the game is done
(check-expect (end-of-the-world (make-WorldState (list (make-Ghost 3000 3000 GHOSTPIC) (make-Ghost 5 5 GHOSTPIC)) 2)) #false)
(check-expect (end-of-the-world (make-WorldState empty 2)) #true)
(define (end-of-the-world ws) (empty? (WorldState-ghosts ws)))

;score-screen: WorldState -> image
;displays the final screen
(check-expect (image? (score-screen (make-WorldState empty 0))) #true)
(define (score-screen ws) (above (text "I ALWAYS KNEW YOU COULD DO IT!" 30 "black")(text (string-append "YOU CAUGHT THEM ALL, ALL " (number->string(WorldState-score ws)) " OF THEM!") 30 "black")))

;ghost-list: number List-of Ghost -> List-of Ghost
;returns the list filled with n Ghosts
(check-expect (length(ghost-list 5 empty)) 5)
(define (ghost-list n lg) (cond[(> n 0)(ghost-list (- n 1)(cons (make-Ghost (random WIDTH) (random HEIGHT) GHOSTPIC)lg))]
                               [else lg]))

(big-bang (make-WorldState (ghost-list INITIALGHOSTAMOUNT empty) 0)
 (on-tick tick)
 (on-mouse mouse-handler)
 (to-draw render)
 (stop-when end-of-the-world score-screen))
