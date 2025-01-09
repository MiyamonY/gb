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

(define (make-memory) (make-vector (k 64)))

(struct -cpu
  (registers memory)
  #:transparent)

(define (make-cpu #:a [a 0] #:b [b 0] #:c [c 0] #:d [d 0] #:e [e 0] #:h [h 0] #:l [l 0] #:sp [sp 0] #:pc [pc 0] #:low-power-mode [low-power-mode #f])
  (-cpu (-registers a b c d e h l sp pc low-power-mode) (make-memory)))

(define (low-power-mode cpu)
  (define registers (-cpu-registers cpu))

  (struct-copy -cpu cpu [registers (struct-copy -registers registers [low-power-mode #t])]))

(define (increment-pc cpu step)
  (define registers (-cpu-registers cpu))

  (define pc (+ step (-registers-pc registers)))

  (struct-copy -cpu cpu [registers (struct-copy -registers registers [pc pc])]))

(define (execute cpu clock code)
  (match code
    [(list 'nop)
     (values (increment-pc cpu 1) (+ 4 clock))]
    [(list 'stop)
     (values (increment-pc (low-power-mode cpu) 1) (+ 4 clock))]
    [(list 'ld-imm16 r16mem imm16)
     (let* ([registers (-cpu-registers cpu)]
            [updated-registers (match r16mem
                                 [(quote bc) (load-bc registers imm16)]
                                 [(quote de) (load-de registers imm16)]
                                 [(quote hl) (load-hl registers imm16)]
                                 [(quote sp) (load-sp registers imm16)])])
       (values (increment-pc (struct-copy -cpu cpu [registers updated-registers]) 3) (+ 12 clock)))]
    [(list 'ld-from-a r16mem)
     (let* ([registers (-cpu-registers cpu)]
            [a (-registers-a registers)]
            [addr (match r16mem
                   [(quote bc) (-registers-bc registers)]
                   [(quote de) (-registers-de registers)]
                   [(quote hl) (-registers-hl registers)]
                   [(quote sp) (-registers-sp registers)])])
       (vector-set! (-cpu-memory cpu) addr a)
       (values (increment-pc cpu 1)  (+ 8 clock)))]))

(define (main)
  (define cpu (cpu (-registers) (make-memory)))

  (define clock 0)

  (execute cpu clock))

(module+ test
  (require rackunit)

  (define (check-cpu? cpu expected)
    (check-equal? (-cpu-registers cpu) (-cpu-registers expected))
    (check-equal? (-cpu-memory cpu) (-cpu-memory expected)))

  (test-case "nop"
    (begin
      (let ([cpu (make-cpu)])
        (let-values ([(cpu clock) (execute cpu 0 '(nop))])
          (check-cpu? cpu (-cpu (-registers 0 0 0 0 0 0 0 0 1 #f) (make-memory)))
          (check-equal? clock 4)
          )
        )
      )
    )

  (test-case "stop"
    (begin
      (let ([cpu (make-cpu)])
        (let-values ([(cpu clock) (execute cpu 0 '(stop))])
          (check-cpu? cpu (-cpu (-registers 0 0 0 0 0 0 0 0 1 #t) (make-memory)))
          (check-equal? clock 4)
          )
        )
      )
    )

  (test-case "load immediatey 16@BC"
    (begin
      (let ([cpu (make-cpu)])
        (let-values ([(cpu clock) (execute cpu 0 '(ld-imm16 bc #x1234))])
          (check-cpu? cpu (-cpu (-registers 0 #x12 #x34 0 0 0 0 0 3 #f) (make-memory)))
          (check-equal? clock 12)
          )
        )
      )
    )

  (test-case "load immediatey 16@DE"
    (begin
      (let ([cpu (make-cpu)])
        (let-values ([(cpu clock) (execute cpu 0 '(ld-imm16 de #x1234))])
          (check-cpu? cpu (-cpu (-registers 0 0 0 #x12 #x34 0 0 0 3 #f) (make-memory)))
          (check-equal? clock 12)
          )
        )
      )
    )

  (test-case "load immediatey 16@HL"
    (begin
      (let ([cpu (make-cpu)])
        (let-values ([(cpu clock) (execute cpu 0 '(ld-imm16 hl #x1234))])
          (check-cpu? cpu (-cpu (-registers 0 0 0 0 0 #x12 #x34 0 3 #f) (make-memory)))
          (check-equal? clock 12)
          )
        )
      )
    )

  (test-case "load immediatey 16@SP"
    (begin
      (let ([cpu (make-cpu)])
        (let-values ([(cpu clock) (execute cpu 0 '(ld-imm16 sp #x1234))])
          (check-cpu? cpu (-cpu (-registers 0 0 0 0 0 0 0 #x1234 3 #f) (make-memory)))
          (check-equal? clock 12)
          )
        )
      )
    )

  (test-case "load from A to [BC]"
    (begin
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
    )

  (test-case "load from A to [DE]"
    (begin
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
    )

  (test-case "load from A to [HL]"
    (begin
      (let ([cpu (make-cpu #:a #xff #:h #x01 #:l #x23)]
            [memory(make-memory)])
        (let-values ([(cpu clock) (execute cpu 0 '(ld-from-a hl))])
          (begin
            (vector-set! memory #x0123 #xff)
            (check-cpu? cpu (-cpu (-registers #xff 0 0 0 0 #x01 #x23 0 1 #f) memory))
            (check-equal? clock 8))
          )
        )
      )
    )

  (test-case "load from A to [sp]"
    (begin
      (let ([cpu (make-cpu #:a #xff #:sp #x123)]
            [memory(make-memory)])
        (let-values ([(cpu clock) (execute cpu 0 '(ld-from-a sp))])
          (begin
            (vector-set! memory #x0123 #xff)
            (check-cpu? cpu (-cpu (-registers #xff 0 0 0 0 0 0 #x0123 1 #f) memory))
            (check-equal? clock 8))
          )
        )
      )
    )
  )
