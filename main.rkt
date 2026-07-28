#lang racket

(struct register (a x y pc sp z n cycle)
  #:mutable
  #:transparent)

(struct cpu (register memory)
  #:mutable
  #:transparent)

(define (neg-byte? byte)
  (not (zero? (bitwise-and byte #x80))))

(define (page-crossed? addr1 addr2)
  (not (zero? (bitwise-and (bitwise-xor addr1 addr2) #xFF00))))

(define (add-addr base diff)
  (define addr (+ base diff))

  (values (modulo addr #x10000) (page-crossed? base addr)))

(define (read-abs-addr-from code-or-memory from)
  (+ (arithmetic-shift (bytes-ref code-or-memory from) 8) (bytes-ref code-or-memory (+ 1 from))))

(define (fetch code cpu)
  (define reg (cpu-register cpu))
  (define pc (register-pc reg))
  (define op-code (bytes-ref code pc))

  (match op-code
    [#xa1
     (set-register-pc! reg (+ pc 2))

     (values `(lda-indirect-x ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xa5
     (set-register-pc! reg (+ pc 2))

     (values `(lda-zero-page ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xa9
     (set-register-pc! reg (+ pc 2))

     (values `(lda-imm ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xad
     (set-register-pc! reg (+ pc 3))

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'lda-abs addr) cpu)]
    [#xb1
     (set-register-pc! reg (+ pc 2))
     (values `(lda-indirect-y ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xb5
     (set-register-pc! reg (+ pc 2))
     (values `(lda-zero-page-x ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xb9
     (set-register-pc! reg (+ pc 3))

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'lda-abs+y addr) cpu)]
    [#xbd
     (set-register-pc! reg (+ pc 3))

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'lda-abs-x addr) cpu)]
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
    [(list 'lda-zero-page-x addr)
     (define x (register-x reg))

     (define val (bytes-ref memory (modulo (+ x addr) #xff)))

     (lda reg val)
     (set-register-cycle! reg 4)
     cpu
     ]
    [(list 'lda-abs addr)
     (define val (bytes-ref memory addr))

     (lda reg val)
     (set-register-cycle! reg 4)
     cpu
     ]
    [(list 'lda-abs-x base-addr)
     (define x (register-x reg))

     (define-values (addr crossed) (add-addr base-addr x))

     (define val (bytes-ref memory addr))

     (lda reg val)
     (set-register-cycle! reg (if crossed 5 4))
     cpu
     ]
    [(list 'lda-indirect-x base-addr)
     (define x (register-x reg))

     (define-values (addr _) (add-addr base-addr x))

     (define memory-addr (read-abs-addr-from memory addr))

     (lda reg (bytes-ref memory memory-addr))
     (set-register-cycle! reg 6)
     cpu
     ]
    [(list 'lda-indirect-y base-addr)
     (define y (register-y reg))

     (define memory-addr (read-abs-addr-from memory base-addr))

     (define-values (addr crossed) (add-addr memory-addr y))

     (lda reg (bytes-ref memory addr))

     (set-register-cycle! reg (if crossed 6 5))

     cpu
     ]
    )
  )

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define (run1 code reg)
      (call-with-values
       (lambda ()
         (define memory (list->bytes (for/list ([i (in-range #x10000)]) (modulo i #x100))))

         (fetch code (cpu reg memory)))
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

     (test-case "0x01"
       (define cpu (run1 #"\xa5\x01" (register 0 0 0 0 0 0 0 0)))

       (check-equal? (cpu-register cpu) (register #x01 0 0 2 0 0 0 3)))))

  (define (lda-zero-page-x)
    (test-suite
     "LDA +x zero page"

     (test-case "0x01"
       (define cpu (run1 #"\xb5\x01" (register 0 1 0 0 0 0 0 0)))

       (check-equal? (cpu-register cpu) (register #x02 1 0 2 0 0 0 4)))))

  (define (lda-abs)
    (test-suite
     "LDA abs"

     (test-case "0x0001"
       (define cpu (run1 #"\xad\x00\x01" (register 0 0 0 0 0 0 0 0)))

       (check-equal? (cpu-register cpu) (register #x01 0 0 3 0 0 0 4)))))

  (define (lda-abs-x)
    (test-suite
     "LDA abs"

     (test-case "0x0001"
       (define cpu (run1 #"\xbd\x00\x01" (register 0 2 0 0 0 0 0 0)))

       (check-equal? (cpu-register cpu) (register #x03 2 0 3 0 0 0 4)))

     (test-case "page crossed"
       (define cpu (run1 #"\xbd\x00\xff" (register 0 2 0 0 0 0 0 0)))

       (check-equal? (cpu-register cpu) (register #x01 2 0 3 0 0 0 5)))))

  (define (lda-indirect-x)
    (test-suite
     "LDA abs"

     (test-case "0x01"
       (define cpu (run1 #"\xa1\x01" (register 0 2 0 0 0 0 0 0)))

       (check-equal? (cpu-register cpu) (register #x04 2 0 2 0 0 0 6)))))

  (define (lda-indirect-y)
    (test-suite
     "LDA abs"

     (test-case "0x01"
       (define cpu (run1 #"\xb1\x01" (register 0 0 3 0 0 0 0 0)))

       (check-equal? (cpu-register cpu) (register #x05 0 3 2 0 0 0 5)))))

  (run-tests
   (test-suite
    "run all"
    (lda-imm-test)
    (lda-zero-page)
    (lda-zero-page-x)
    (lda-abs)
    (lda-abs-x)
    (lda-indirect-x)
    (lda-indirect-y)))
)
