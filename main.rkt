#lang racket

(struct register (a x y pc sp z n cycle)
  #:mutable
  #:transparent)

(struct cpu (register memory)
  #:mutable
  #:transparent)

(define (neg-byte? byte)
  (not (zero? (bitwise-and byte #x80))))

(define (fetch code cpu)
  (define reg (cpu-register cpu))
  (define pc (register-pc reg))
  (define op-code (bytes-ref code pc))

  (match op-code
    [#xa9
     (set-register-pc! reg (+ pc 2))

     (values `(lda-imm ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xa5
     (set-register-pc! reg (+ pc 2))

     (values `(lda-zero-page ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xb5
     (set-register-pc! reg (+ pc 2))
     (values `(lda+x-zero-page ,(bytes-ref code (+ 1 pc))) cpu)]
    ))

(define (decode op cpu)
  (define memory (cpu-memory cpu))
  (define reg (cpu-register cpu))

  (define (lda reg val)
    (set-register-a! reg val)
    (set-register-z! reg (if (= val 0) 1 0))
    (set-register-n! reg (if (neg-byte? val) 1 0)))

  (match op
    [(list 'lda-imm imm)
     (lda reg imm)
     (set-register-cycle! reg 2)
     cpu
     ]
    [(list 'lda-zero-page addr)
     (define val (bytes-ref memory addr))

     (lda reg val)
     (set-register-cycle! reg 3)
     cpu
     ]
    [(list 'lda+x-zero-page addr)
     (define x (register-x reg))

     (define val (bytes-ref memory (modulo (+ x addr) #xff)))

     (lda reg val)
     (set-register-cycle! reg 4)
     cpu
     ]
    )
  )

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define (run1 code reg)
      (call-with-values
       (lambda () (fetch code (cpu reg #"\x01\x02\x03\x04")))
       decode))

  (define (lda-imm-test)
    (test-suite
     "LDA imm"

     (test-case "0xff"
       (define cpu (run1 #"\xa9\xff" (register 0 0 0 0 0 0 0 0)))

       (check-equal? (cpu-register cpu) (register #xff 0 0 2 0 0 1 2)))

     (test-case "0x00"
       (define cpu (run1 #"\xa9\x00" (register 0 0 0 0 0 0 0 0)))

       (check-equal? (cpu-register cpu) (register #x00 0 0 2 0 1 0 2)))))

  (define (lda-zero-page)
    (test-suite
     "LDA zero page"

     (test-case "0xff"
       (define cpu (run1 #"\xa5\x01" (register 0 0 0 0 0 0 0 0)))

       (check-equal? (cpu-register cpu) (register #x02 0 0 2 0 0 0 3)))))

  (define (lda+x-zero-page)
    (test-suite
     "LDA +x zero page"

     (test-case "0xff"
       (define cpu (run1 #"\xb5\x01" (register 0 1 0 0 0 0 0 0)))

       (check-equal? (cpu-register cpu) (register #x03 1 0 2 0 0 0 4)))))

  (run-tests
   (test-suite
    "run all"
    (lda-imm-test)
    (lda-zero-page)
    (lda+x-zero-page)))
)
