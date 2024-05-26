(import (scheme base)
        (scheme inexact)
        (hoot debug)
        (hoot ffi)
        (hoot hashtables)
        (hoot match)
        (dom canvas)
        (dom document)
        (dom element)
        (dom event)
        (dom image)
        (dom media)
        (dom window)
        (dom debug)
        (math)
        (math rect)
        (math vector))

;;;
;;; Missing from Hoot
;;;
(define-syntax push!
  (syntax-rules ()
    ((_ var val)
     (set! var (cons val var)))))

(define-syntax pop!
  (syntax-rules ()
    ((_ var)
     (let ((top (car var)))
       (set! var (cdr var))
       top))))

(define (filter f list)
  (match list
    (() '())
    ((h . t)
     (if (f h)
         (cons h (filter f t))
         (filter f t)))))

(define (identity x) x)

;; ~srfi-43 vector-for-each
(define (vector-for-each1 f v)
  (let ((len (vector-length v)))
    (let loop ((i 0))
      (unless (= i len)
        (f i (vector-ref v i))
        (loop (+ i 1))))))

;;;
;;; Game
;;;

(define game-width    640.0)
(define game-height   480.0)
(define tick-interval 0.01)
(define %pi (acos -1))

;; speed of sound in water in NM/sec
(define speed-of-sound 0.81)

;; radius at which pings age out
(define ping-max-range 2.0)

(define (current-time)
  (/ (current-time-ms) 1000.0))

(define (polar-vec2 magnitude angle)
  (vec2 (* magnitude (cos angle))
        (* magnitude (sin angle))))

(define-record-type <ping>
  (make-ping origin radius samples)
  ping?
  (origin ping-origin)
  (radius ping-radius set-ping-radius!)
  (samples ping-samples))

(define (make-ping* origin)
  (make-ping origin 0 (make-vector 100 #t)))

(define-record-type <player>
  (make-player loc direction)
  player?
  (loc player-loc set-player-loc!)
  (direction player-direction set-player-direction!))

(define-record-type <entity>
  (make-entity origin radius)
  entity?
  (origin entity-origin)
  (radius entity-radius))


;;;
;;; Main Loops
;;;

(define pings '())

(define last-tick-time (current-time))
(define last-ping-time (current-time))

(define (tick)
  (let* ((time (current-time))
         (interval (- time last-tick-time)))

    (set! pings
          (filter identity
                  (map (lambda (ping)
                         (set-ping-radius! ping
                                           (+ (ping-radius ping) (* speed-of-sound interval)))
                         (if (> (ping-radius ping) ping-max-range)
                             #f
                             ping))
                       pings)))

    (when (> time (+ last-ping-time 1.0))
      (push! pings (make-ping* (vec2 (* 1 (- (random) 0.5))
                                     (* 1 (- (random) 0.5)))))
      (set! last-ping-time time))

    (set! last-tick-time time)))

(define tick-callback (procedure->external tick))

(define last-frame-time (current-time))

(define view-origin (vec2 0.0 0.0))
(define view-radius 1.0)

(define (project-point! point)
  (vec2-mul-scalar! point (* game-height 0.5 (/ 1 view-radius)))
  (vec2-add! point (vec2 (/ game-width 2) (/ game-height 2))))

(define (draw-ping ping)
  (set-fill-color! context "#ff0000")
  (vector-for-each1
   (lambda (i v)
     (let* ((angle (* 2 %pi (/ i (vector-length (ping-samples ping)))))
            (loc (polar-vec2 (ping-radius ping) angle)))

       (vec2-add! loc (ping-origin ping))
       (project-point! loc)

       (fill-rect context (vec2-x loc) (vec2-y loc) 1 1)))
   (ping-samples ping)))

(define (draw frame-ms)
  (let ((frame-time (/ frame-ms 1000)))
    (set-fill-color! context "#000000")
    (fill-rect context 0 0 game-width game-height)

    (set-fill-color! context "#ffffff")


    ;; (set-font! context "bold 12px monospace")
    ;; (set-text-align! context "left")
    ;; (fill-text context (string-append (number->string (/ 1 (- frame-time last-frame-time)))
    ;;                                   " FPS")
    ;;            20.0 20.0)


    ;; (for-each draw-ping pings)

    (set-fill-color! context "#ffffff")
    (set-stroke-color! context "#ffffff")

    (begin-path context)
    (ellipse context 100.0 100.0 50.0 75.0 (/ %pi 4) 0.0 (* 2 %pi) 0)
    ;; (ellipse context 100 100 50 75 (/ %pi 4) 0 (* 2 %pi) 0)
    ;; (rect context 100 200 300 400)
    ;; (rect context 100 200 400 400)
    (close-path context)
    ;; (stroke context)
    (fill context "evenodd")

    (set! last-frame-time frame-time)

    (request-animation-frame draw-callback)))
(define draw-callback (procedure->external draw))

(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))
(set-element-width! canvas (exact game-width))
(set-element-height! canvas (exact game-height))

(request-animation-frame draw-callback)
(interval tick-callback tick-interval)
