#lang racket

(struct register (a x y pc sp z n)
  #:mutable
  #:transparent)

(define (fetch code reg)
  (define pc (register-pc reg))
  (define op-code (bytes-ref code pc))

  (match op-code
    [#xa9
     (set-register-pc! reg (+ pc 2))
     (values `(lda-imm ,(bytes-ref code (+ 1 pc))) reg)]))

(define (neg-byte? byte)
  (not (zero? (bitwise-and byte #x80))))

(define (decode op reg)
  (match op
    [(list 'lda-imm imm)
     (set-register-a! reg imm)
     (set-register-z! reg (if (= imm 0) 1 0))
     (set-register-n! reg (if (neg-byte? imm) 1 0))
     reg
     ]
    )
  )

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define (run1 code)
      (call-with-values
       (lambda () (fetch code (register 0 0 0 0 0 0 0)))
       decode))

  (define (lda-imm-test)
    (test-suite
     "LDA imm"
     (test-case "0xff"
       (define reg (run1 #"\xa9\xff"))
       (check-equal? reg (register #xff 0 0 2 0 0 1)))

     (test-case "0x00"
       (define reg (run1 #"\xa9\x00"))
       (check-equal? reg (register 0 0 0 2 0 1 0)))))

  (run-tests (lda-imm-test))
)
