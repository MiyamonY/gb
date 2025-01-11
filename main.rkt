#lang racket
(require (for-syntax racket/syntax))

(define (k size) (* size 1024))

(define (make-memory) (make-vector (k 64)))

(struct -cpu
  ([a #:mutable]
   [b #:mutable]
   [c #:mutable]
   [d #:mutable]
   [e #:mutable]
   [h #:mutable]
   [l #:mutable]
   [f #:mutable]
   [sp #:mutable]
   [pc #:mutable]
   [low-power-mode #:mutable]
   memory)
  #:transparent)

(define (make-cpu #:a [a 0] #:b [b 0] #:c [c 0] #:d [d 0] #:e [e 0] #:h [h 0] #:l [l 0] #:f [f 0] #:sp [sp 0] #:pc [pc 0] #:low-power-mode [low-power-mode #f] #:memory [memory (make-memory)])
  (-cpu a b c d e h l f sp pc low-power-mode memory))

(define-syntax (define-16bit-register stx)
  (syntax-case stx ()
    [(_ reg-hi reg-lo)
     (with-syntax ([id-get (format-id #'reg-hi "-cpu-~a~a" #'reg-hi #'reg-lo)]
                   [id-set (format-id #'reg-hi "load-~a~a" #'reg-hi #'reg-lo)]
                   [set-high-register! (format-id #'reg-hi "set--cpu-~a!" #'reg-hi)]
                   [high-register (format-id #'reg-hi "-cpu-~a" #'reg-hi)]
                   [set-low-register! (format-id #'reg-lo "set--cpu-~a!" #'reg-lo)]
                   [low-register (format-id #'reg-lo "-cpu-~a" #'reg-lo)])
       #'(begin
           (define (id-get cpu)
             (+ (* (high-register cpu) (expt 2 8)) (low-register cpu)))

           (define (id-set cpu imm16)
             (define high (quotient imm16 (expt 2 8)))
             (define low (remainder imm16 (expt 2 8)))
             (set-high-register! cpu high)
             (set-low-register! cpu low))))]))

(define-16bit-register b c)

(define-16bit-register d e)

(define-16bit-register h l)

(define (load-sp cpu imm16)
  (set--cpu-sp! cpu imm16))

(define (load-pc cpu imm16)
  (set--cpu-pc! cpu imm16))

(define (low-power-mode cpu)
  (set--cpu-low-power-mode! cpu #t))

(define-syntax (define-flag stx)
  (syntax-case stx ()
    [(_ name bit)
     (with-syntax ([id-set (format-id #'name "set-flag-~a" #'name)]
                   [id-clear (format-id #'name "clear-flag-~a" #'name)])

       #'(begin
           (define (id-set cpu)
             (define flag (-cpu-f cpu))
             (set--cpu-f! cpu (bitwise-ior flag (arithmetic-shift #x01 bit))))

           (define (id-clear cpu)
             (define flag (-cpu-f cpu))
             (set--cpu-f! cpu (bitwise-and flag (bitwise-xor #xff (arithmetic-shift #x01 bit)))))))]))

(define-flag z 7)

(define-flag n 6)

(define-flag h 5)

(define-flag c 4)

(define-syntax (define-increment/decrement-8bit-register stx)
  (syntax-case stx ()
    [(_ r8)
     (with-syntax ([id-access-register (format-id #'r8 "-cpu-~a" #'r8)]
                   [id-load-register (format-id #'r8 "-cpu-~a" #'r8)]
                   [id-increnment (format-id #'r8 "increment-~a" #'r8)]
                   [id-decrement (format-id #'r8 "decrement-~a" #'r8)])

       #'(begin
           (define (id-increnment cpu [val 1])
             (define r8+n (remainder (+ (id-access-register cpu) val) #xffff))

             (define cpu (if (= r8+n 0) (set-flag-z cpu) (clear-flag-z cpu)))

             (id-load-register cpu r8+n))

           (define (id-decrement cpu [val 1])
             (define r8-n (remainder (+ (- (id-access-register cpu) val) #xffff) #xffff))

             (define cpu (if (= r8-n 0) (set-flag-z cpu) (clear-flag-z cpu)))

             (id-load-register cpu r8-n))))]))

(define-increment/decrement-8bit-register a)

(define-increment/decrement-8bit-register b)

(define-syntax (define-increment/decrement-16bit-register stx)
  (syntax-case stx ()
    [(_ r16)
     (with-syntax ([id-access-register (format-id #'r16 "-cpu-~a" #'r16)]
                   [id-load-register (format-id #'r16 "load-~a" #'r16)]
                   [id-increnment (format-id #'r16 "increment-~a" #'r16)]
                   [id-decrement (format-id #'r16 "decrement-~a" #'r16)])

       #'(begin
           (define (id-increnment cpu [val 1])
             (define r16+n (remainder (+ (id-access-register cpu) val) #xffff))

             (id-load-register cpu r16+n))

           (define (id-decrement cpu [val 1])
             (define r16-n (remainder (+ (- (id-access-register cpu) val) #xffff) #xffff))

             (id-load-register cpu r16-n))))]))

(define-increment/decrement-16bit-register bc)

(define-increment/decrement-16bit-register de)

(define-increment/decrement-16bit-register hl)

(define-increment/decrement-16bit-register sp)

(define-increment/decrement-16bit-register pc)

(define (fetch-and-decode cpu)
  (define pc (-cpu-pc cpu))

  (define instruction (vector-ref (-cpu-memory cpu) pc))

  (case instruction
    [(#x00) '(nop)]
    [(#x10) '(stop)]
    [(#x01) `(ld-imm16 bc ,(vector-ref (-cpu-memory cpu) (+ pc 1)))]
    [(#x11) `(ld-imm16 de ,(vector-ref (-cpu-memory cpu) (+ pc 1)))]
    [(#x21) `(ld-imm16 hl ,(vector-ref (-cpu-memory cpu) (+ pc 1)))]
    [(#x31) `(ld-imm16 sp ,(vector-ref (-cpu-memory cpu) (+ pc 1)))]
    [(#x02) '(ld-from-a bc)]
    [(#x12) '(ld-from-a de)]
    [(#x22) '(ld-from-a hl+)]
    [(#x32) '(ld-from-a hl-)]
    [(#x03) '(inc bc)]
    [(#x13) '(inc de)]
    [(#x23) '(inc hl)]
    [(#x33) '(inc sp)]
    [else (error 'fetch-and-decode "Unknown instruction@~a" instruction)])
  )

(define (execute cpu clock instruction)
  (match instruction
    [(list 'nop)
     (increment-pc cpu 1)
     (+ 4 clock)]
    [(list 'stop)
     (low-power-mode cpu)
     (increment-pc cpu 1)
     (+ 4 clock)]
    [(list 'ld-imm16 r16mem imm16)
     (match r16mem
       [(quote bc) (load-bc cpu imm16)]
       [(quote de) (load-de cpu imm16)]
       [(quote hl) (load-hl cpu imm16)]
       [(quote sp) (load-sp cpu imm16)])
     (increment-pc cpu 3)
     (+ 12 clock)]
    [(list 'ld-from-a r16mem)
     (define a (-cpu-a cpu))
     (define addr
       (match r16mem
         [(quote bc) (-cpu-bc cpu)]
         [(quote de) (-cpu-de cpu)]
         [(quote hl+) (let [(hl (-cpu-hl cpu))] (increment-hl cpu) hl)]
         [(quote hl-) (let [(hl (-cpu-hl cpu))] (decrement-hl cpu) hl)]))

     (vector-set! (-cpu-memory cpu) addr a)
     (increment-pc cpu 1)
     (+ 8 clock)]
    [(list 'inc r16)
     (match r16
       [(quote bc) (increment-bc cpu)]
       [(quote de) (increment-de cpu)]
       [(quote hl) (increment-hl cpu)]
       [(quote sp) (increment-sp cpu)])
     (increment-pc cpu 1)
     (+ 8 clock)]))

(define (main)
  (define cpu (make-cpu))

  (define clock 0)

  (execute cpu clock))

(module+ test
  (require rackunit)

  (define (check-cpu? cpu expected)
    (check-equal? cpu expected))

  (test-case "decode nop"
    (let* ([cpu (make-cpu)]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(nop))))

  (test-case "decode stop"
    (let* ([cpu (make-cpu #:memory #(#x10))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(stop))
      )
    )

  (test-case "decode load immediatey 16@BC"
    (let* ([cpu (make-cpu #:memory #(#x01 #xff))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(ld-imm16 bc #xff))
      )
    )

  (test-case "decode load immediatey 16@DE"
    (let* ([cpu (make-cpu #:memory #(#x11 #xff))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(ld-imm16 de #xff))
      )
    )

  (test-case "decode load immediatey 16@HL"
    (let* ([cpu (make-cpu #:memory #(#x21 #xff))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(ld-imm16 hl #xff))
      )
    )

  (test-case "decode load immediatey 16@SP"
    (let* ([cpu (make-cpu #:memory #(#x31 #xff))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(ld-imm16 sp #xff))
      )
    )

  (test-case "decode load from a to [BC]"
    (let* ([cpu (make-cpu #:memory #(#x02))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(ld-from-a bc))
      )
    )

  (test-case "decode load from a to [DE]"
    (let* ([cpu (make-cpu #:memory #(#x12))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(ld-from-a de))
      )
    )

  (test-case "decode load from a to [HL+]"
    (let* ([cpu (make-cpu #:memory #(#x22))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(ld-from-a hl+))
      )
    )

  (test-case "decode load from a to [HL-]"
    (let* ([cpu (make-cpu #:memory #(#x32))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(ld-from-a hl-))
      )
    )

  (test-case "increment BC"
    (let* ([cpu (make-cpu #:memory #(#x03))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(inc bc))
      )
    )

  (test-case "increment DE"
    (let* ([cpu (make-cpu #:memory #(#x13))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(inc de))
      )
    )

  (test-case "increment HL"
    (let* ([cpu (make-cpu #:memory #(#x23))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(inc hl))
      )
    )

  (test-case "increment SP"
    (let* ([cpu (make-cpu #:memory #(#x33))]
           [instruction (fetch-and-decode cpu)])
      (check-equal? instruction '(inc sp))
      )
    )

  (test-case "decode error"
    (let ([cpu (make-cpu #:memory #(#xd3))])
      (check-exn #rx"Unknown instruction@211" (lambda () (fetch-and-decode cpu)))
      )
    )

  (test-case "nop"
    (let ([cpu (make-cpu)])
      (let ([clock (execute cpu 0 '(nop))])
        (check-cpu? cpu (make-cpu #:pc #x0001))
        (check-equal? clock 4)
        )
      )
    )

  (test-case "stop"
    (let ([cpu (make-cpu)])
      (let ([clock (execute cpu 0 '(stop))])
        (check-cpu? cpu (make-cpu #:pc #x0001 #:low-power-mode #t))
        (check-equal? clock 4)
        )
      )
    )

  (test-case "clear flag z"
    (let ([cpu (make-cpu #:f #xff)])
      (clear-flag-z cpu)
      (check-cpu? cpu (make-cpu #:f #x7f ))
      )
    )

  (test-case "set flag z"
    (let ([cpu (make-cpu #:f #x00)])
      (set-flag-z cpu)
      (check-cpu? cpu (make-cpu #:f #x80))
      )
    )

  (test-case "clear flag n"
    (let ([cpu (make-cpu #:f #xff)])
      (clear-flag-n cpu)
      (check-cpu? cpu (make-cpu #:f #xbf))
      )
    )

  (test-case "set flag n"
    (let ([cpu (make-cpu #:f #x00)])
      (set-flag-n cpu)
      (check-cpu? cpu (make-cpu #:f #x40))))

  (test-case "load immediatey 16@BC"
    (let ([cpu (make-cpu)])
      (let ([clock (execute cpu 0 '(ld-imm16 bc #x1234))])
        (check-cpu? cpu (make-cpu #:b #x12 #:c #x34 #:pc #x0003))
        (check-equal? clock 12)
        )
      )
    )

  (test-case "load immediatey 16@DE"
    (let ([cpu (make-cpu)])
      (let ([clock (execute cpu 0 '(ld-imm16 de #x1234))])
        (check-cpu? cpu (make-cpu #:d #x12 #:e #x34 #:pc #x0003))
        (check-equal? clock 12)
        )
      )
    )

  (test-case "load immediatey 16@HL"
    (let ([cpu (make-cpu)])
      (let ([clock (execute cpu 0 '(ld-imm16 hl #x1234))])
        (check-cpu? cpu (make-cpu #:h #x12 #:l #x34  #:pc #x0003))
        (check-equal? clock 12)
        )
      )
    )

  (test-case "load immediatey 16@SP"
    (let ([cpu (make-cpu)])
      (let ([clock (execute cpu 0 '(ld-imm16 sp #x1234))])
        (check-cpu? cpu (make-cpu #:sp #x1234 #:pc #x0003))
        (check-equal? clock 12)
        )
      )
    )

  (test-case "load from A to [BC]"
    (let ([cpu (make-cpu #:a #xff #:b #x01 #:c #x23)]
          [memory(make-memory)])
      (let ([clock (execute cpu 0 '(ld-from-a bc))])
        (begin
          (vector-set! memory #x0123 #xff)
          (check-cpu? cpu (make-cpu #:a #xff #:b #x01 #:c #x23 #:pc #x0001 #:memory memory))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "load from A to [DE]"
    (let ([cpu (make-cpu #:a #xff #:d #x01 #:e #x23)]
          [memory(make-memory)])
      (let ([clock (execute cpu 0 '(ld-from-a de))])
        (begin
          (vector-set! memory #x0123 #xff)
          (check-cpu? cpu (make-cpu #:a #xff #:d #x01 #:e #x23 #:pc #x0001 #:memory memory))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "load from A to [HL+]"
    (let ([cpu (make-cpu #:a #xff #:h #x01 #:l #x23)]
          [memory(make-memory)])
      (let ([clock (execute cpu 0 '(ld-from-a hl+))])
        (begin
          (vector-set! memory #x0123 #xff)
          (check-cpu? cpu (make-cpu  #:a #xff #:h #x01 #:l #x24  #:pc #x0001  #:memory memory))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "load from A to [HL-]"
    (let ([cpu (make-cpu #:a #xff #:h #x1 #:l #x23)]
          [memory (make-memory)])
      (let ([clock (execute cpu 0 '(ld-from-a hl-))])
        (begin
          (vector-set! memory #x0123 #xff)
          (check-cpu? cpu (make-cpu #:a #xff #:h #x01 #:l #x22 #:pc 1 #:memory memory))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "increment BC"
    (let ([cpu (make-cpu #:c #xff)])
      (let ([clock (execute cpu 0 '(inc bc))])
        (begin
          (check-cpu? cpu (make-cpu #:b #x01 #:c #x00 #:pc #x0001))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "increment DE"
    (let ([cpu (make-cpu #:e #xff)])
      (let ([clock (execute cpu 0 '(inc de))])
        (begin
          (check-cpu? cpu (make-cpu #:d #x01 #:e #x00 #:pc #x0001))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "increment HL"
    (let ([cpu (make-cpu #:l #xff)])
      (let ([clock (execute cpu 0 '(inc hl))])
        (begin
          (check-cpu? cpu (make-cpu #:h #x01 #:l #x00 #:pc #x0001))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "increment SP"
    (let ([cpu (make-cpu #:sp #xffff)])
      (let ([clock (execute cpu 0 '(inc sp))])
        (begin
          (check-cpu? cpu (make-cpu #:pc #x0001 #:sp 1))
          (check-equal? clock 8))
        )
      )
    )
  )
