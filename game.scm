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

(define image:player (make-image "assets/images/player.png"))

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

(define (make-ping* origin radius)
  (make-ping origin radius (make-vector 100 #t)))

(define-record-type <entity>
  (make-entity origin radius direction data)
  entity?
  (origin entity-origin set-entity-origin!)
  (radius entity-radius)
  (direction entity-direction set-entity-direction!)
  (data entity-data))

(define-record-type <player>
  (make-player)
  player?)

(define-record-type <rock>
  (make-rock)
  rock?)

(define-record-type <echo>
  (make-echo origin time)
  echo?
  (origin echo-origin)
  (time echo-time))

;;;
;;; Tick Loop
;;;

(define pings '())

(define echoes '())

(define tick-time (current-time))
(define last-tick-time (current-time))

(define player (make-entity (vec2 -0.1 0) 0.05 0.0 (make-player)))
(define player-thrust 0.0)
(define player-steering 0.0)

(define rocks
  (list (make-entity (polar-vec2 0.5 (/ %pi 4)) 0.1 0 (make-rock))
        (make-entity (polar-vec2 0.3 (- (/ %pi 4))) 0.1 0 (make-rock))
        (make-entity (polar-vec2 0.4 (* 0.8 %pi)) 0.15 0 (make-rock))))

(define entities (cons player rocks))

;;; The angle from l to r, in [0, 2 pi)
(define (angle l r)
  (let ((v (vec2-copy r)))
    (vec2-sub! v l)
    (atan (vec2-y v) (vec2-x v))))

(define (distance v1 v2)
  (let ((v1p (vec2-copy v1)))
    (vec2-sub! v1p v2)
    (vec2-magnitude v1p)))

(define (check-ping-samples! ping entity step)
  (let ((angle0 (angle (ping-origin ping) (entity-origin entity)))
        (nsamples (vector-length (ping-samples ping))))
    (define (index-for angle)
      (modulo (exact (floor (* nsamples (/ angle (* 2 %pi)))))
              nsamples))
    (define (point-for index)
      (let ((p (polar-vec2 (ping-radius ping)
                           (* 2 %pi (/ index nsamples)))))
        (vec2-add! p (ping-origin ping))
        p))

    (let iter ((index (index-for angle0)))
      (let* ((point (point-for index))
             (diff (vec2-copy point)))
        (vec2-sub! diff (entity-origin entity))

        (cond
         ((> (vec2-magnitude diff) (entity-radius entity))
          '())

         ((vector-ref (ping-samples ping) index)
          (vector-set! (ping-samples ping) index #f)
          (push! echoes (make-echo point tick-time))
          (iter (modulo (+ index step) nsamples)))

         (#t
          (iter (modulo (+ index step) nsamples))))))
    ))

(define (ping-intersect! ping)
  (for-each
   (lambda (entity)
     (when (and (> (ping-radius ping)
                   (- (distance (ping-origin ping) (entity-origin entity)) (entity-radius entity)))
                (< (ping-radius ping)
                   (+ (distance (ping-origin ping) (entity-origin entity)) (entity-radius entity))))
       (check-ping-samples! ping entity 1)
       (check-ping-samples! ping entity -1)))
   entities))

(define ping-interval 2.0)
(define last-ping-time (current-time))

(define (tick)
  (set! tick-time (current-time))
  (let* ((interval (- tick-time last-tick-time)))

    (set! pings
          (filter identity
                  (map (lambda (ping)
                         (set-ping-radius! ping
                                           (+ (ping-radius ping) (* speed-of-sound interval)))
                         (ping-intersect! ping)
                         (if (> (ping-radius ping) ping-max-range)
                             #f
                             ping))
                       pings)))

    (when (> tick-time (+ last-ping-time ping-interval))
      (push! pings (make-ping* (vec2-copy (entity-origin player)) (entity-radius player)))
      (set! last-ping-time tick-time))

    (set-entity-direction! player (+ (entity-direction player)
                                     (* player-steering interval)))
    (vec2-add! (entity-origin player)
               (polar-vec2 (* player-thrust interval)
                           (entity-direction player)))

    (set! last-tick-time tick-time)))

(define tick-callback (procedure->external tick))

(define thrust-magnitude 0.2)
(define steering-magnitude 1.5)
(define (on-key-down event)
  (let ((code (keyboard-event-code event)))
    (cond
     ((string=? code "KeyS")
      (set! player-thrust (- (/ thrust-magnitude 2))))
     ((string=? code "KeyW")
      (set! player-thrust thrust-magnitude))
     ((string=? code "KeyK")
      (set! player-steering (- steering-magnitude)))
     ((string=? code "Semicolon")
      (set! player-steering steering-magnitude)))))

(define (on-key-up event)
  (let ((code (keyboard-event-code event)))
    (cond
     ((or (string=? code "KeyS")
          (string=? code "KeyW"))
      (set! player-thrust 0.0))

     ((or (string=? code "KeyK")
          (string=? code "Semicolon"))
      (set! player-steering 0.0)))))

(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
(add-event-listener! (current-document) "keyup"
                     (procedure->external on-key-up))
;;;
;;; Rendering
;;;

(define frame-time (current-time))
(define last-frame-time (current-time))

(define view-origin (vec2 0.0 0.0))
(define view-radius 1.0)

(define (project-point! point)
  (vec2-sub! point view-origin)
  (vec2-mul-scalar! point (* game-height 0.5 (/ 1 view-radius)))
  (vec2-add! point (vec2 (/ game-width 2) (/ game-height 2))))

(define (view-scale n)
  (* game-height (/ n view-radius 2)))

(define (draw-ping ping)
  (set-fill-color! context "#ff8000")
  ;; (vector-for-each1
  ;;  (lambda (i v)
  ;;    (when v
  ;;      (let* ((angle (* 2 %pi (/ i (vector-length (ping-samples ping)))))
  ;;             (loc (polar-vec2 (ping-radius ping) angle)))

  ;;        (vec2-add! loc (ping-origin ping))
  ;;        (project-point! loc)

  ;;        (fill-rect context (vec2-x loc) (vec2-y loc) 1 1))))
  ;;  (ping-samples ping))

  (set-stroke-color! context "#ff8000")

  (let ((center (vec2-copy (ping-origin ping)))
        (radius (/ (view-scale (ping-radius ping)) 2)))
    (project-point! center)
    (begin-path context)
    (ellipse context (vec2-x center) (vec2-y center) radius radius 0.0 0.0 (* 2 %pi) 0)
    (stroke context)))

(define (draw-rock rock)
  (let ((pos (vec2-copy (entity-origin rock)))
        (size (view-scale (entity-radius rock))))
    (project-point! pos)

    (set-stroke-color! context "#ffffff")

    (begin-path context)
    (ellipse context (vec2-x pos) (vec2-y pos) size size 0.0 0.0 (* 2 %pi) 0)
    (stroke context)))

(define max-echo-age 2.5)

(define (draw-echo echo)
  (let ((render-time (+ (echo-time echo) (distance (entity-origin player) (echo-origin echo))))
        (pos (vec2-copy (echo-origin echo))))
    (project-point! pos)
    (cond
     ((< (+ render-time max-echo-age) frame-time)
      #f)

     ((< render-time frame-time)
      (set-fill-color! context "#ffffff")
      (fill-rect context (- (vec2-x pos) 2.5) (- (vec2-y pos) 2.5) 5 5)
      echo)

     (#t echo))))

(define (draw frame-ms)
  (set! frame-time (/ frame-ms 1000))

  (set-fill-color! context "#000000")
  (fill-rect context 0 0 game-width game-height)

  ;; player
  (set! view-origin (entity-origin player))
  (let ((pos (vec2-copy (entity-origin player)))
        (angle (entity-direction player))
        (screen-size 15.0))
    (project-point! pos)
    (save-context context)
    (translate context (vec2-x pos) (vec2-y pos))
    (rotate context (+ angle (/ %pi 2))            )
    (draw-image context image:player 0.0 0.0 64.0 64.0 (- screen-size) (- screen-size) (* 2 screen-size) (* 2 screen-size))
    (restore-context context))

  ;; sonar
  (for-each draw-ping pings)

  (set! echoes (filter identity (map draw-echo echoes)))

  ;; rocks (for debugging)
  ;; (for-each draw-rock rocks)

  ;; panel
  (set-fill-color! context "#404040")

  (begin-path context)
  (rect context 0.0 0.0 game-width game-height)
  (ellipse context (/ game-width 2) (/ game-height 2) (* game-height 0.48) (* game-height 0.48) 0.0 0.0 (* 2 %pi) 0)
  (close-path context)
  (fill context "evenodd")

  ;; debug
  ;; (set-fill-color! context "#ffffff")
  ;; (set-font! context "bold 12px monospace")
  ;; (set-text-align! context "left")
  ;; (fill-text context (string-append (number->string (/ 1 (- frame-time last-frame-time)))
  ;;                                   " FPS")
  ;;            20.0 20.0)

  (set! last-frame-time frame-time)

  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))
(set-element-width! canvas (exact game-width))
(set-element-height! canvas (exact game-height))

(request-animation-frame draw-callback)
(interval tick-callback tick-interval)
