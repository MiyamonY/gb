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

(define (update-flag-when-incrementing cpu val incr)
  (clear-flag-n cpu)

  (if (= val #x100)
      (set-flag-z cpu)
      (clear-flag-z cpu))

  (if (and (bitwise-bit-set? val 4) (>= #x10 (- val incr)))
      (set-flag-h cpu)
      (clear-flag-h cpu)))

(define (update-flag-when-decrementing cpu val decr)
  (set-flag-n cpu)

  (if (= val #x00)
      (set-flag-z cpu)
      (clear-flag-z cpu))

  (if (and (not (bitwise-bit-set? val 4)) (bitwise-bit-set? (+ val decr) 4))
      (clear-flag-h cpu)
      (set-flag-h cpu)))

(define-syntax (define-increment/decrement-8bit-register stx)
  (syntax-case stx ()
    [(_ r8)
     (with-syntax ([id-access-register (format-id #'r8 "-cpu-~a" #'r8)]
                   [id-load-register (format-id #'r8 "set--cpu-~a!" #'r8)]
                   [id-increnment (format-id #'r8 "increment-~a" #'r8)]
                   [id-decrement (format-id #'r8 "decrement-~a" #'r8)])

       #'(begin
           (define (id-increnment cpu [incr 1])
             (define val (+ (id-access-register cpu) incr))

             (update-flag-when-incrementing cpu val incr)

             (id-load-register cpu (remainder val #x100)))

           (define (id-decrement cpu [decr 1])
             (define r8-n (remainder (+ (- (id-access-register cpu) decr) #xffff) #xffff))

             (update-flag-when-incrementing cpu val decr)

             (id-load-register cpu r8-n))))]))

(define-increment/decrement-8bit-register a)

(define-increment/decrement-8bit-register b)

(define-increment/decrement-8bit-register d)

(define-increment/decrement-8bit-register h)

(define (increment-indirect-hl cpu [incr 1])
  (define addr (-cpu-hl cpu))

  (define memory (-cpu-memory cpu))

  (define val (+ (vector-ref memory addr) incr))

  (update-flag-when-incrementing cpu val incr)

  (vector-set! memory addr (remainder val #x100)))

(define (decrement-indirect-hl cpu [decr 1])
  (define addr (-cpu-hl cpu))

  (define memory (-cpu-memory cpu))

  (define val (- (vector-ref memory addr) decr))

  (update-flag-when-decrementing cpu val decr)

  (vector-set! memory addr (remainder val #x100)))

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
    [(#x03) '(inc-r16 bc)]
    [(#x13) '(inc-r16 de)]
    [(#x23) '(inc-r16 hl)]
    [(#x33) '(inc-r16 sp)]
    [(#x04) '(inc-r8 b)]
    [(#x14) '(inc-r8 d)]
    [(#x24) '(inc-r8 h)]
    [(#x34) '(inc-r8 [hl])]
    [(#x05) '(dec-r8 b)]
    [(#x15) '(dec-r8 d)]
    [(#x25) '(dec-r8 h)]
    [(#x35) '(dec-r8 [hl])]
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
    [(list 'inc-r16 r16)
     (match r16
       [(quote bc) (increment-bc cpu)]
       [(quote de) (increment-de cpu)]
       [(quote hl) (increment-hl cpu)]
       [(quote sp) (increment-sp cpu)])
     (increment-pc cpu 1)
     (+ 8 clock)]
    [(list 'inc-r8 r8)
     (define clock+
       (match r8
         [(quote b) (increment-b cpu) 4]
         [(quote d) (increment-d cpu) 4]
         [(quote h) (increment-h cpu) 4]
         [(quote [hl]) (increment-indirect-hl cpu) 12]))
     (increment-pc cpu 1)
     (+ clock+ clock)]
    [(list 'dec-r8 r8)
     (define clock+
       (match r8
         [(quote b) (decrement-b cpu) 4]
         [(quote d) (decrement-d cpu) 4]
         [(quote h) (decrement-h cpu) 4]
         [(quote [hl]) (decrement-indirect-hl cpu) 12]))
     (increment-pc cpu 1)
     (+ clock+ clock)]))

(define (main)
  (define cpu (make-cpu))

  (define clock 0)

  (execute cpu clock))

(module+ test
  (require rackunit)

  (define (check-cpu? cpu expected)
    (check-equal? cpu expected))

  (test-case "decode nop"
    (define instruction (fetch-and-decode (make-cpu)))

    (check-equal? instruction '(nop)))

  (test-case "decode stop"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x10))))

    (check-equal? instruction '(stop)))

  (test-case "decode load immediatey 16@BC"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x01 #xff))))

    (check-equal? instruction '(ld-imm16 bc #xff)))

  (test-case "decode load immediatey 16@DE"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x11 #xff))))

    (check-equal? instruction '(ld-imm16 de #xff)))

  (test-case "decode load immediatey 16@HL"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x21 #xff))))

    (check-equal? instruction '(ld-imm16 hl #xff)))

  (test-case "decode load immediatey 16@SP"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x31 #xff))))

    (check-equal? instruction '(ld-imm16 sp #xff)))

  (test-case "decode load from a to [BC]"
    (define cpu (make-cpu #:memory #(#x02)))
    (define instruction (fetch-and-decode cpu))

    (check-equal? instruction '(ld-from-a bc)))

  (test-case "decode load from a to [DE]"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x12))))

    (check-equal? instruction '(ld-from-a de)))

  (test-case "decode load from a to [HL+]"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x22))))

    (check-equal? instruction '(ld-from-a hl+)))

  (test-case "decode load from a to [HL-]"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x32))))

    (check-equal? instruction '(ld-from-a hl-)))

  (test-case "increment BC"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x03))))

    (check-equal? instruction '(inc-r16 bc)))

  (test-case "increment DE"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x13))))

    (check-equal? instruction '(inc-r16 de)))

  (test-case "increment HL"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x23))))

    (check-equal? instruction '(inc-r16 hl)))

  (test-case "increment SP"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x33))))

    (check-equal? instruction '(inc-r16 sp)))

  (test-case "increment B"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x04))))

    (check-equal? instruction '(inc-r8 b)))

  (test-case "increment D"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x14))))

    (check-equal? instruction '(inc-r8 d)))

  (test-case "increment H"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x24))))

    (check-equal? instruction '(inc-r8 h)))

  (test-case "increment [HL]"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x34))))

    (check-equal? instruction '(inc-r8 [hl])))

  (test-case "decrement B"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x05))))

    (check-equal? instruction '(dec-r8 b)))

  (test-case "decrement D"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x15))))

    (check-equal? instruction '(dec-r8 d)))

  (test-case "decrement H"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x25))))

    (check-equal? instruction '(dec-r8 h)))

  (test-case "decrement [HL]"
    (define instruction (fetch-and-decode (make-cpu #:memory #(#x35))))

    (check-equal? instruction '(dec-r8 [hl])))

  (test-case "decode error"
    (check-exn #rx"Unknown instruction@211"
               (lambda () (fetch-and-decode (make-cpu #:memory #(#xd3))))))

  (test-case "nop"
    (define cpu (make-cpu))

    (define clock (execute cpu 0 '(nop)))

    (check-cpu? cpu (make-cpu #:pc #x0001))
    (check-equal? clock 4))

  (test-case "stop"
    (define cpu (make-cpu))
    (define clock (execute cpu 0 '(stop)))

    (check-cpu? cpu (make-cpu #:pc #x0001 #:low-power-mode #t))
    (check-equal? clock 4))

  (test-case "load immediatey 16@BC"
    (define cpu (make-cpu))
    (define clock (execute cpu 0 '(ld-imm16 bc #x1234)))

    (check-cpu? cpu (make-cpu #:b #x12 #:c #x34 #:pc #x0003))
    (check-equal? clock 12))

  (test-case "load immediatey 16@DE"
    (define cpu (make-cpu))
    (define clock (execute cpu 0 '(ld-imm16 de #x1234)))

    (check-cpu? cpu (make-cpu #:d #x12 #:e #x34 #:pc #x0003))
    (check-equal? clock 12))

  (test-case "load immediatey 16@HL"
    (define cpu (make-cpu))
    (define clock (execute cpu 0 '(ld-imm16 hl #x1234)))

    (check-cpu? cpu (make-cpu #:h #x12 #:l #x34  #:pc #x0003))
    (check-equal? clock 12))

  (test-case "load immediatey 16@SP"
    (define cpu (make-cpu))
    (define clock (execute cpu 0 '(ld-imm16 sp #x1234)))

    (check-cpu? cpu (make-cpu #:sp #x1234 #:pc #x0003))
    (check-equal? clock 12))

  (test-case "load from A to [BC]"
    (define cpu (make-cpu #:a #xff #:b #x01 #:c #x23))
    (define memory (make-memory))
    (vector-set! memory #x0123 #xff)
    (define clock (execute cpu 0 '(ld-from-a bc)))

    (check-cpu? cpu (make-cpu #:a #xff #:b #x01 #:c #x23 #:pc #x0001 #:memory memory))
    (check-equal? clock 8))

  (test-case "load from A to [DE]"
    (define cpu (make-cpu #:a #xff #:d #x01 #:e #x23))
    (define memory (make-memory))
    (vector-set! memory #x0123 #xff)
    (define clock (execute cpu 0 '(ld-from-a de)))

    (check-cpu? cpu (make-cpu #:a #xff #:d #x01 #:e #x23 #:pc #x0001 #:memory memory))
    (check-equal? clock 8))

  (test-case "load from A to [HL+]"
    (define cpu (make-cpu #:a #xff #:h #x01 #:l #x23))
    (define memory (make-memory))
    (vector-set! memory #x0123 #xff)
    (define clock (execute cpu 0 '(ld-from-a hl+)))

    (check-cpu? cpu (make-cpu  #:a #xff #:h #x01 #:l #x24  #:pc #x0001  #:memory memory))
    (check-equal? clock 8))

  (test-case "load from A to [HL-]"
    (define cpu (make-cpu #:a #xff #:h #x1 #:l #x23))
    (define memory (make-memory))
    (vector-set! memory #x0123 #xff)
    (define clock (execute cpu 0 '(ld-from-a hl-)))

    (check-cpu? cpu (make-cpu #:a #xff #:h #x01 #:l #x22 #:pc 1 #:memory memory))
    (check-equal? clock 8))

  (test-case "increment BC"
    (define cpu (make-cpu #:c #xff))
    (define clock (execute cpu 0 '(inc-r16 bc)))

    (check-cpu? cpu (make-cpu #:b #x01 #:c #x00 #:pc #x0001))
    (check-equal? clock 8))

  (test-case "increment DE"
    (define cpu (make-cpu #:e #xff))
    (define clock (execute cpu 0 '(inc-r16 de)))

    (check-cpu? cpu (make-cpu #:d #x01 #:e #x00 #:pc #x0001))
    (check-equal? clock 8))

  (test-case "increment HL"
    (define cpu (make-cpu #:l #xff))
    (define clock (execute cpu 0 '(inc-r16 hl)))

    (check-cpu? cpu (make-cpu #:h #x01 #:l #x00 #:pc #x0001))
    (check-equal? clock 8))

  (test-case "increment SP"
    (define cpu (make-cpu #:sp #xffff))
    (define clock (execute cpu 0 '(inc-r16 sp)))

    (check-cpu? cpu (make-cpu #:pc #x0001 #:sp 1))
    (check-equal? clock 8))

  (test-case "increment B"
    (define cpu (make-cpu #:b #xff #:f #b01000000))
    (define clock (execute cpu 0 '(inc-r8 b)))

    (check-cpu? cpu (make-cpu #:b #x00 #:f #b10000000 #:pc #x0001))
    (check-equal? clock 4))

  (test-case "increment B with half carry"
    (define cpu (make-cpu #:b #x0f #:f #b01000000))
    (define clock (execute cpu 0 '(inc-r8 b)))

    (check-cpu? cpu (make-cpu #:b #x10 #:f #b00100000 #:pc #x0001))
    (check-equal? clock 4))

  (test-case "increment D"
    (define cpu (make-cpu #:d #xff #:f #b01000000))
    (define clock (execute cpu 0 '(inc-r8 d)))

    (check-cpu? cpu (make-cpu #:d #x00 #:f #b10000000 #:pc #x0001))
    (check-equal? clock 4))

  (test-case "increment D"
    (define cpu (make-cpu #:h #xff #:f #b01000000))
    (define clock (execute cpu 0 '(inc-r8 h)))

    (check-cpu? cpu (make-cpu #:h  #x00 #:f #b10000000 #:pc #x0001))
    (check-equal? clock 4))

  (test-case "increment [HL]"
    (define cpu (make-cpu #:h #x00 #:l #x01 #:f #b01000000 #:memory (vector-copy #(#xff #xff)))) ;to mutable
    (define clock (execute cpu 0 '(inc-r8 [hl])))

    (check-cpu? cpu (make-cpu #:h #x00 #:l #x01 #:f #b10000000 #:memory #(#xff #x00) #:pc #x0001))
    (check-equal? clock 12))

  (test-case "decrement B"
    (define cpu (make-cpu #:b #x00 #:f #b01000000))
    (define clock (execute cpu 0 '(dec-r8 b)))

    (check-cpu? cpu (make-cpu #:b #xff #:f #b00000000 #:pc #x0001))
    (check-equal? clock 4))

  (test-case "increment B with half carry"
    (define cpu (make-cpu #:b #x0f #:f #b01000000))
    (define clock (execute cpu 0 '(inc-r8 b)))

    (check-cpu? cpu (make-cpu #:b #x10 #:f #b00100000 #:pc #x0001))
    (check-equal? clock 4))

  (test-case "increment D"
    (define cpu (make-cpu #:d #xff #:f #b01000000))
    (define clock (execute cpu 0 '(inc-r8 d)))

    (check-cpu? cpu (make-cpu #:d #x00 #:f #b10000000 #:pc #x0001))
    (check-equal? clock 4))

  (test-case "increment D"
    (define cpu (make-cpu #:h #xff #:f #b01000000))
    (define clock (execute cpu 0 '(inc-r8 h)))

    (check-cpu? cpu (make-cpu #:h  #x00 #:f #b10000000 #:pc #x0001))
    (check-equal? clock 4))

  (test-case "increment [HL]"
    (define cpu (make-cpu #:h #x00 #:l #x01 #:f #b01000000 #:memory (vector-copy #(#xff #xff)))) ;to mutable
    (define clock (execute cpu 0 '(inc-r8 [hl])))

    (check-cpu? cpu (make-cpu #:h #x00 #:l #x01 #:f #b10000000 #:memory #(#xff #x00) #:pc #x0001))
    (check-equal? clock 12))
  )
