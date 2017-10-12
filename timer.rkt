#lang racket


;;;-------------------------------------------------------------------------------------------------
;;; requires

(require 2htdp/universe)
(require 2htdp/image
         (only-in racket/gui/base play-sound))


;;;-------------------------------------------------------------------------------------------------
;;; variables

;;; constants
(define N-SIZE 20)
(define WIDTH 115)
(define HEIGHT 27)
(define N-COLOR "green")
(define B-COLOR "black")
(define BG (rectangle WIDTH HEIGHT "solid" B-COLOR))
(define PAUSED? #t)
(define P-COUNT 0)
(define REST? #f)

;;; structure
(struct timer (m s) #:mutable #:transparent)


;;;-------------------------------------------------------------------------------------------------
;;; functions

;;; timer -> timer
(define (tick-tock t)
  (let ([time-m (timer-m t)]
        [time-s (timer-s t)])
    (cond [PAUSED? t]
          [(and (>= time-m 0) (> time-s 0))
           (set-timer-s! t (- time-s 1)) t]
          [(and (> time-m 0) (<= time-s 0))
           (set-timer-m! t (- time-m 1))
           (set-timer-s! t 59) t]
          [(and (= time-m 0) (= time-s 0))
           (cond [(and (equal? REST? #f) (< P-COUNT 4)) (set! P-COUNT (+ P-COUNT 1))
                                                        (play-sound "alert.wav" #t)
                                                        (set! REST? #t)                   
                                                        (set! N-COLOR "red")
                                                        (timer 5 0)]
                 [(and (equal? REST? #t) (<= P-COUNT 4)) (set! REST? #f)
                                                         (play-sound "alert.wav" #t)
                                                         (set! N-COLOR "green")
                                                         (timer 25 0)]
                 [(and (equal? REST? #f) (= P-COUNT 4)) (set! P-COUNT 0)
                                                        (play-sound "alert.wav" #t)
                                                        (set! REST? #t)
                                                        (set! N-COLOR "red")
                                                        (timer 25 0)])]
          [else t])))

;; timer -> image
(define (render t)
  (overlay (text (string-append (number->string (timer-m t))
                                ":"
                                (number->string (timer-s t))
                                " PC: "
                                (number->string P-COUNT))
                 N-SIZE N-COLOR)
           BG))

;;; timer keyevent -> timer
(define (key-handle t k)
  (cond [(key=? k "r")
         (set! PAUSED? #t)
         (set! N-COLOR "green")
         (set! P-COUNT 0)
         (timer 25 0)]
        [(key=? k "s")
         (set! PAUSED? #f)
         t]
        [else t]))

;;; entry point
(define main
  (big-bang (timer 25 0)
            (to-draw render)
            (on-tick tick-tock 1)
            (on-key key-handle)))

;;; unit tests
(module+ test
  (require rackunit)
  (check-equal? (key-handle (timer 0 0) "r") (timer 25 0))
  (check-equal? (key-handle (timer 0 9) "r") (timer 25 0))
  (check-equal? (key-handle (timer 9 0) "r") (timer 25 0))
  (check-equal? (key-handle (timer 9 9) "r") (timer 25 0))
  (check-equal? (key-handle (timer 0 0) " ") (timer 0 0))
  (check-equal? (key-handle (timer 0 9) " ") (timer 0 9))
  (check-equal? (key-handle (timer 9 0) " ") (timer 9 0))
  (check-equal? (key-handle (timer 9 9) " ") (timer 9 9))
  (check-equal? (render (timer 0 0))
                (overlay (text (string-append (number->string 0)
                                              ":"
                                              (number->string 0)
                                              " PC: "
                                              (number->string P-COUNT))
                               N-SIZE N-COLOR) BG)))
