#lang racket
(require rsound)
(require ffi/vector)

(define sample-rate 44100)
(define twopi (* 2 pi))
(define nyquist (/ sample-rate 2))
;; length of generated test sounds
(define length (* sample-rate 2))
;; the buffer we hold onto and edit
(define prev (box '()))

;; s16 vector functions
(define (s16vector-map! proc v)
  (for ([i (s16vector-length v)])
    (s16vector-set! v i (real->s16 (proc (s16->real (s16vector-ref v i)))))))

;; counted map, function of val index -> new val
(define (s16vector-cmap! proc v)
   (for ([i (s16vector-length v)])
    (s16vector-set! v i (real->s16 (proc (s16->real (s16vector-ref v i)) i)))))

;; structure of a sine partial
(struct sine (frequency volume))

;; Creates a sine function given a frequency and volume
(define (generate-sine-function frequency volume)
  ;; safety anti-aliasing!
  (lambda (x) (* volume (sin (* twopi frequency (/ x sample-rate))))))

;; Converts a sine struct to a function
(define (sine-to-function sine)
    (generate-sine-function (sine-frequency sine) (sine-volume sine)))

;; Converts a sine list to a function
(define (sine-list-to-function sine-list)
    (let ([function-list (map sine-to-function
                              ;; efficiency anti-aliasing
                              (filter (lambda (s) (<= (sine-frequency s) nyquist)) sine-list))])
      (lambda (x) (foldr + 0 (map (lambda (f) (f x)) function-list)))))

;; helper for build sine list, keeps track of the current partial number
(define (bslh num freq volume freq-factor exponent n)
    (if (>= n num)
        '()
        (cons (sine (+ (* freq (expt exponent (sub1 n))) (* (sub1 n) freq freq-factor)) (/ volume n))
              (bslh num freq volume freq-factor exponent (add1 n)))))

;; Builds a list of sine partials
;;    num: the number of partials to build
;;    volume: amplitude of fundamental (values around .1 work best)
;;    freq: frequency of the fundamental
;;    freq-factor: hz value added to consecutive partials
;;    exponent: hz value multiplied to consecutive partials
(define (build-sine-list num freq volume freq-factor exponent)
    (bslh num freq volume freq-factor exponent 1))

;; example sound where partials are a fourth away from each other
;; (play (build-sound length (sine-list-to-function (build-sine-list 128 50 .05 0 4/3))))


(define (gen length function)
  (let ([new (build-sound length function)])
    (play new)
    (set-box! prev new)))

(define (prev-buffer)
  (rsound-data (unbox prev)))

(define (set-to-buf buf)
  (set-box! prev (vec->rsound buf sample-rate)))

(define (e-func function)
  (let ([buf (prev-buffer)])
  (for ([i (s16vector-length buf)])
    (s16vector-set! (rsound-data (unbox prev)) i (real->s16 (function (s16->real (s16vector-ref buf i))))))
  (hear)))

(define (e-iirs lod)
  (apply-delays (prev-buffer) lod)
  (hear))

(define (e-iir d)
  (apply-delays (prev-buffer) (list d))
  (hear))

(define (e-firs lod)
  (set-to-buf (apply-delays-fir (prev-buffer) lod))
  (hear))

(define (e-fir d)
  (set-to-buf (apply-delays-fir (prev-buffer) (list d)))
  (hear))

(define (hear)
  (play (unbox prev)))

(define (dist f)
  (lambda (x) (* (if (< x 0) -1 1) (min 1 (f (abs x))))))

(define basic-dist
  (dist sqrt))

;; example sound, almost a saw wave but with some exponential offset
(gen length (sine-list-to-function (build-sine-list 512 50 .1 1 1.01)))

(struct delay (time amp))

(define (apply-delays buf lod)
  (for ([i (s16vector-length buf)])
    (s16vector-set! buf i (real->s16
                     (foldr (lambda (cur acc)
                              ;; make sure its not before the sound starts
                              (if (> 0 (- i (delay-time cur)))                             
                                  acc
                              (+ acc
                                 (* (delay-amp cur) (s16->real (s16vector-ref buf (- i (delay-time cur))))))))
                            (s16->real (s16vector-ref buf i))
                            lod)))))

(define (apply-delays-fir buf lod)
  (let ([new-buf (make-s16vector (s16vector-length buf) 0)])
  (for ([i (s16vector-length buf)])
    (s16vector-set! new-buf i (real->s16
                     (foldr (lambda (cur acc)
                              ;; make sure its not before the sound starts
                              (if (> 0 (- i (delay-time cur)))
                                  acc
                              (+ acc
                                 (* (delay-amp cur) (s16->real (s16vector-ref buf (- i (delay-time cur))))))))
                            (s16->real (s16vector-ref buf i))
                            lod))))
    new-buf))

;; TODO: function to get fractional index of buffer using sinc interpolation
;; read about fir filters and the sinc function

(define (index-of-buf buf i)
  0)

(define (sinc x)
  (if (= x 0)
    1
    (/ (sin (* pi x)) (* pi x))))

(define (build-sinc-buffer length)
  (let ([buf (make-s16vector length 0)])
    1))
  
  
    