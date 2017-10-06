#lang racket

(require 2htdp/universe)
(require 2htdp/image
         (only-in racket/gui/base play-sound))


;;; constants
(define N-SIZE 20)
(define WIDTH 75)
(define HEIGHT 27)
(define N-COLOR "white")
(define B-COLOR "black")
(define BG (rectangle WIDTH HEIGHT "solid" B-COLOR))
(define PAUSED? #t)


;;; data
(struct timer (m s) #:mutable #:transparent)


;;; functions
;;; timer -> timer
(define (tick-tock t)
  (cond [PAUSED? t]
        [(and (>= (timer-m t) 0) (> (timer-s t) 0))
         (set-timer-s! t (- (timer-s t) 1)) t]
        [(and (> (timer-m t) 0) (<= (timer-s t) 0))
         (set-timer-m! t (- (timer-m t) 1)) (set-timer-s! t 59) t]
        [(and (= (timer-m t) 0) (= (timer-s t) 0))
         (play-sound "alert.wav" #t) t]
        [else t]))

;; timer -> image
(define (render t)
  (overlay (text (string-append (number->string (timer-m t))
                                ":"
                                (number->string (timer-s t)))
                 N-SIZE N-COLOR)
           BG))

;;; timer keyevent -> timer
(define (key-handle t k)
  (cond [(key=? k "r") (set-timer-m! t 30) (set-timer-s! t 0) t]
        [(key=? k "p") (set! PAUSED? (not PAUSED?)) t]
        [else t]))

;;; entry point
(define main
  (big-bang (timer 30 0)
            (to-draw render)
            (on-tick tick-tock 1)
            (on-key key-handle)))


;;; unit tests
(module+ test
  (require rackunit)
  ;(check-equal? (tick-tock (timer 30 0)) (timer 30 0))
  (check-equal? (key-handle (timer 0 0) "r") (timer 30 0))
  (check-equal? (key-handle (timer 0 9) "r") (timer 30 0))
  (check-equal? (key-handle (timer 9 0) "r") (timer 30 0))
  (check-equal? (key-handle (timer 9 9) "r") (timer 30 0))
  (check-equal? (key-handle (timer 0 0) " ") (timer 0 0))
  (check-equal? (key-handle (timer 0 9) " ") (timer 0 9))
  (check-equal? (key-handle (timer 9 0) " ") (timer 9 0))
  (check-equal? (key-handle (timer 9 9) " ") (timer 9 9))
  (check-equal? (render (timer 0 0))
                (overlay (text (string-append (number->string 0)
                                              ":"
                                              (number->string 0))
                               N-SIZE N-COLOR) BG)))
