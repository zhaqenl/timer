#lang racket
(require 2htdp/universe)
(require 2htdp/image
         (only-in racket/gui/base play-sound))
(require rackunit)

;; Constants

(define N-SIZE 20)
(define B-SIZE 60)
(define N-COLOR "green")
(define B-COLOR "black")
(define BG (circle B-SIZE "solid" B-COLOR))

;; Data

(struct timer (m s) #:mutable #:transparent)

;; Functions

;; Timer -> Timer

(define (tick-tock t)
  (cond [(and (>= (timer-m t) 0) (> (timer-s t) 0)) (set-timer-s! t (- (timer-s t) 1)) t]
        [(and (> (timer-m t) 0) (<= (timer-s t) 0))  (set-timer-m! t (- (timer-m t) 1)) (set-timer-s! t 59) t]
        [(and (= (timer-m t) 0) (= (timer-s t) 0)) (play-sound "alert1.wav" #t) t]
        [else t]))

(check-equal? (tick-tock (timer 30 0)) (timer 29 59))

;; Timer -> Image

(define (render t) (overlay (text (string-append (number->string (timer-m t)) ":" (number->string (timer-s t))) N-SIZE N-COLOR) BG))

(check-equal? (render (timer 0 0)) (overlay (text (string-append (number->string 0) ":" (number->string 0)) N-SIZE N-COLOR) BG))

;; Timer KeyEvent -> Timer

(define (key-handle t k)
  (cond [(key=? k "r") (set-timer-m! t 30) (set-timer-s! t 0) t]
        [else t]))

(check-equal? (key-handle (timer 0 0) "r") (timer 30 0))
(check-equal? (key-handle (timer 0 9) "r") (timer 30 0))
(check-equal? (key-handle (timer 9 0) "r") (timer 30 0))
(check-equal? (key-handle (timer 9 9) "r") (timer 30 0))
(check-equal? (key-handle (timer 0 0) " ") (timer 0 0))
(check-equal? (key-handle (timer 0 9) " ") (timer 0 9))
(check-equal? (key-handle (timer 9 0) " ") (timer 9 0))
(check-equal? (key-handle (timer 9 9) " ") (timer 9 9))


(define main
  (big-bang (timer 30 0)
            (to-draw render)
            (on-tick tick-tock 1)
            (on-key key-handle)))