#lang racket
(require (for-syntax racket/syntax))

(define (k size) (* size 1024))

(struct -registers
  (a
   b
   c
   d
   e
   h
   l
   sp
   pc
   low-power-mode)
  #:transparent)

(define-syntax (define-16bit-register stx)
  (syntax-case stx ()
    [(_ reg-hi reg-lo)
     (with-syntax ([id-get (format-id #'reg-hi "-registers-~a~a" #'reg-hi #'reg-lo)]
                   [id-set (format-id #'reg-hi "load-~a~a" #'reg-hi #'reg-lo)]
                   [high-register (format-id #'reg-hi "~a" #'reg-hi)]
                   [-registers-high-register (format-id #'reg-hi "-registers-~a" #'reg-hi)]
                   [low-register (format-id #'reg-lo "~a" #'reg-lo)]
                   [-registers-low-register (format-id #'reg-lo "-registers-~a" #'reg-lo)])
       #'(begin
           (define (id-get registers)
             (+ (* (-registers-high-register registers) (expt 2 8)) (-registers-low-register registers)))

           (define (id-set registers imm16)
             (define high (quotient imm16 (expt 2 8)))
             (define low (remainder imm16 (expt 2 8)))
             (struct-copy -registers registers [high-register high] [low-register low]))))]))

(define-16bit-register b c)

(define-16bit-register d e)

(define-16bit-register h l)

(define (load-sp registers imm16)
  (struct-copy -registers registers [sp imm16]))

(define (load-pc registers imm16)
  (struct-copy -registers registers [pc imm16]))

(define (make-memory) (make-vector (k 64)))

(struct -cpu
  (registers memory)
  #:transparent)

(define (make-cpu #:a [a 0] #:b [b 0] #:c [c 0] #:d [d 0] #:e [e 0] #:h [h 0] #:l [l 0] #:sp [sp 0] #:pc [pc 0] #:low-power-mode [low-power-mode #f] #:memory [memory (make-memory)])
  (-cpu (-registers a b c d e h l sp pc low-power-mode) memory))

(define (low-power-mode cpu)
  (define registers (-cpu-registers cpu))

  (struct-copy -cpu cpu [registers (struct-copy -registers registers [low-power-mode #t])]))

(define-syntax (define-increment/decrement-register stx)
  (syntax-case stx ()
    [(_ r16)
     (with-syntax ([id-access-register (format-id #'r16 "-registers-~a" #'r16)]
                   [id-load-register (format-id #'r16 "load-~a" #'r16)]
                   [id-increnment (format-id #'r16 "increment-~a" #'r16)]
                   [id-decrement (format-id #'r16 "decrement-~a" #'r16)])

       #'(begin
           (define (id-increnment cpu [val 1])
             (define registers (-cpu-registers cpu))

             (define r16++ (remainder (+ (id-access-register registers) val) #xffff))

             (struct-copy -cpu cpu [registers (id-load-register registers r16++)]))

           (define (id-decrement cpu [val 1])
             (define registers (-cpu-registers cpu))

             (define r16-- (remainder (+ (- (id-access-register registers) val) #xffff) #xffff))

             (struct-copy -cpu cpu [registers (id-load-register registers r16--)]))))]))

(define-increment/decrement-register bc)

(define-increment/decrement-register de)

(define-increment/decrement-register hl)

(define-increment/decrement-register sp)

(define-increment/decrement-register pc)

(define (fetch-and-decode cpu)
  (define pc (-registers-pc (-cpu-registers cpu)))

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
     (values (increment-pc cpu 1) (+ 4 clock))]
    [(list 'stop)
     (values (increment-pc (low-power-mode cpu) 1) (+ 4 clock))]
    [(list 'ld-imm16 r16mem imm16)
     (define registers (-cpu-registers cpu))
     (define registers+ (match r16mem
                          [(quote bc) (load-bc registers imm16)]
                          [(quote de) (load-de registers imm16)]
                          [(quote hl) (load-hl registers imm16)]
                          [(quote sp) (load-sp registers imm16)]))
     (values (increment-pc (struct-copy -cpu cpu [registers registers+]) 3) (+ 12 clock))]
    [(list 'ld-from-a r16mem)
     (define registers (-cpu-registers cpu))
     (define a (-registers-a registers))
     (define-values (addr cpu+)
       (match r16mem
         [(quote bc) (values (-registers-bc registers) cpu)]
         [(quote de) (values (-registers-de registers) cpu)]
         [(quote hl+) (values (-registers-hl registers) (increment-hl cpu))]
         [(quote hl-) (values (-registers-hl registers) (decrement-hl cpu))]))

     (vector-set! (-cpu-memory cpu+) addr a)
     (values (increment-pc cpu+ 1)  (+ 8 clock))]
    [(list 'inc r16)
     (define cpu+ (match r16
                   [(quote bc) (increment-bc cpu)]
                   [(quote de) (increment-de cpu)]
                   [(quote hl) (increment-hl cpu)]
                   [(quote sp) (increment-sp cpu)]))
     (values (increment-pc cpu+ 1) (+ 8 clock))]))

(define (main)
  (define cpu (cpu (-registers) (make-memory)))

  (define clock 0)

  (execute cpu clock))

(module+ test
  (require rackunit)

  (define (check-cpu? cpu expected)
    (check-equal? (-cpu-registers cpu) (-cpu-registers expected))
    (check-equal? (-cpu-memory cpu) (-cpu-memory expected)))

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
      (let-values ([(cpu clock) (execute cpu 0 '(nop))])
        (check-cpu? cpu (-cpu (-registers 0 0 0 0 0 0 0 0 1 #f) (make-memory)))
        (check-equal? clock 4)
        )
      )
    )

  (test-case "stop"
    (let ([cpu (make-cpu)])
      (let-values ([(cpu clock) (execute cpu 0 '(stop))])
        (check-cpu? cpu (-cpu (-registers 0 0 0 0 0 0 0 0 1 #t) (make-memory)))
        (check-equal? clock 4)
        )
      )
    )

  (test-case "load immediatey 16@BC"
    (let ([cpu (make-cpu)])
      (let-values ([(cpu clock) (execute cpu 0 '(ld-imm16 bc #x1234))])
        (check-cpu? cpu (-cpu (-registers 0 #x12 #x34 0 0 0 0 0 3 #f) (make-memory)))
        (check-equal? clock 12)
        )
      )
    )

  (test-case "load immediatey 16@DE"
    (let ([cpu (make-cpu)])
      (let-values ([(cpu clock) (execute cpu 0 '(ld-imm16 de #x1234))])
        (check-cpu? cpu (-cpu (-registers 0 0 0 #x12 #x34 0 0 0 3 #f) (make-memory)))
        (check-equal? clock 12)
        )
      )
    )

  (test-case "load immediatey 16@HL"
    (let ([cpu (make-cpu)])
      (let-values ([(cpu clock) (execute cpu 0 '(ld-imm16 hl #x1234))])
        (check-cpu? cpu (-cpu (-registers 0 0 0 0 0 #x12 #x34 0 3 #f) (make-memory)))
        (check-equal? clock 12)
        )
      )
    )

  (test-case "load immediatey 16@SP"
    (let ([cpu (make-cpu)])
      (let-values ([(cpu clock) (execute cpu 0 '(ld-imm16 sp #x1234))])
        (check-cpu? cpu (-cpu (-registers 0 0 0 0 0 0 0 #x1234 3 #f) (make-memory)))
        (check-equal? clock 12)
        )
      )
    )

  (test-case "load from A to [BC]"
    (let ([cpu (make-cpu #:a #xff #:b #x01 #:c #x23)]
          [memory(make-memory)])
      (let-values ([(cpu clock) (execute cpu 0 '(ld-from-a bc))])
        (begin
          (vector-set! memory #x0123 #xff)
          (check-cpu? cpu (-cpu (-registers #xff #x01 #x23 0 0 0 0 0 1 #f) memory))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "load from A to [DE]"
    (let ([cpu (make-cpu #:a #xff #:d #x01 #:e #x23)]
          [memory(make-memory)])
      (let-values ([(cpu clock) (execute cpu 0 '(ld-from-a de))])
        (begin
          (vector-set! memory #x0123 #xff)
          (check-cpu? cpu (-cpu (-registers #xff 0 0 #x01 #x23 0 0 0 1 #f) memory))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "load from A to [HL+]"
    (let ([cpu (make-cpu #:a #xff #:h #x01 #:l #x23)]
          [memory(make-memory)])
      (let-values ([(cpu clock) (execute cpu 0 '(ld-from-a hl+))])
        (begin
          (vector-set! memory #x0123 #xff)
          (check-cpu? cpu (-cpu (-registers #xff 0 0 0 0 #x01 #x24 0 1 #f) memory))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "load from A to [HL-]"
    (let ([cpu (make-cpu #:a #xff #:h #x1 #:l #x23)]
          [memory (make-memory)])
      (let-values ([(cpu clock) (execute cpu 0 '(ld-from-a hl-))])
        (begin
          (vector-set! memory #x0123 #xff)
          (check-cpu? cpu (-cpu (-registers #xff 0 0 0 0 #x01 #x22 0 1 #f) memory))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "increment BC"
    (let ([cpu (make-cpu #:c #xff)]
          [memory (make-memory)])
      (let-values ([(cpu clock) (execute cpu 0 '(inc bc))])
        (begin
          (check-cpu? cpu (-cpu (-registers 0 #x01 #x00 0 0 0 0 0 1 #f) memory))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "increment DE"
    (let ([cpu (make-cpu #:e #xff)]
          [memory (make-memory)])
      (let-values ([(cpu clock) (execute cpu 0 '(inc de))])
        (begin
          (check-cpu? cpu (-cpu (-registers 0 0 0 #x01 0 0 0 0 1 #f) memory))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "increment HL"
    (let ([cpu (make-cpu #:l #xff)]
          [memory (make-memory)])
      (let-values ([(cpu clock) (execute cpu 0 '(inc hl))])
        (begin
          (check-cpu? cpu (-cpu (-registers 0 0 0 0 0 #x01 #x00 0 1 #f) memory))
          (check-equal? clock 8))
        )
      )
    )

  (test-case "increment SP"
    (let ([cpu (make-cpu #:sp #xffff)]
          [memory (make-memory)])
      (let-values ([(cpu clock) (execute cpu 0 '(inc sp))])
        (begin
          (check-cpu? cpu (-cpu (-registers 0 0 0 0 0 0 0 #x0001 1 #f) memory))
          (check-equal? clock 8))
        )
      )
    )
  )
