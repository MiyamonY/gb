#lang racket
(require (for-syntax syntax/parse
                     racket/syntax))

(struct register (a x y pc sp z n cycle) #:mutable #:transparent)

(define/contract (step-register-cycle! register diff)
  (-> register? (integer-in 1 10) void)
  (define cycle (register-cycle register))

  (set-register-cycle! register (+ cycle diff)))

(define/contract (step-register-pc! register diff)
  (-> register? (integer-in 1 3) void)

  (define pc (register-pc register))

  (set-register-pc! register (+ pc diff)))

(define/contract (lda reg val)
  (-> register? (integer-in 0 #xff) void)

  (set-register-a! reg val)
  (set-register-z! reg (if (= val 0) 1 0))
  (set-register-n! reg (if (neg-byte? val) 1 0)))

(define/contract (ldx reg val)
  (-> register? (integer-in 0 #xff) void)
  (set-register-x! reg val)
  (set-register-z! reg (if (= val 0) 1 0))
  (set-register-n! reg (if (neg-byte? val) 1 0)))

(define/contract (ldy reg val)
  (-> register? (integer-in 0 #xff) void)

  (set-register-y! reg val)
  (set-register-z! reg (if (= val 0) 1 0))
  (set-register-n! reg (if (neg-byte? val) 1 0)))

(struct cpu (register memory) #:mutable #:transparent)

(define/contract (neg-byte? byte)
  (-> (integer-in #x00 #xff) boolean?)

  (not (zero? (bitwise-and byte #x80))))

(define/contract (page-crossed? addr1 addr2)
  (-> (integer-in 0 #x10000) (integer-in 0 #x20000) boolean?)

  (not (zero? (bitwise-and (bitwise-xor addr1 addr2) #xFF00))))

(define/contract (add-addr base diff)
  (-> (integer-in 0 #xffff) (integer-in 0 #xffff) (values (integer-in 0 #xffff) boolean?))

  (define addr (+ base diff))

  (values (modulo addr #x10000) (page-crossed? base addr)))

(define/contract (add-addr-in-zero-page base diff)
  (-> (integer-in 0 #x00ff) (integer-in 0 #xff) (values (integer-in 0 #x00ff) boolean?))

  (define addr (+ base diff))

  (values (modulo addr #x0100) (page-crossed? base addr)))

(define/contract (read-abs-addr-from code-or-memory from)
  (-> bytes? integer? integer?)

  (+ (arithmetic-shift (bytes-ref code-or-memory from) 8) (bytes-ref code-or-memory (+ 1 from))))

(define/contract (write-to-memory cpu start bytes)
  (-> cpu? integer? bytes? void?)

  (define memory (cpu-memory cpu))

  (bytes-copy! memory start bytes))

(define/contract (read-from-memory cpu start size)
  (-> cpu? integer? integer? bytes?)

  (define memory (cpu-memory cpu))

  (subbytes memory start (+ start size)))

(define/contract (fetch code cpu)
  (-> bytes? cpu? (values any/c cpu?))

  (define reg (cpu-register cpu))
  (define pc (register-pc reg))
  (define op-code (bytes-ref code pc))

  (match op-code
    [#x81
     (step-register-pc! reg 2)

     (values `(sta-indirect-x ,(bytes-ref code (+ 1 pc))) cpu)]
    [#x84
     (step-register-pc! reg 2)

     (values `(sty-zero-page ,(bytes-ref code (+ 1 pc))) cpu)]
    [#x85
     (step-register-pc! reg 2)

     (values `(sta-zero-page ,(bytes-ref code (+ 1 pc))) cpu)]
    [#x86
     (step-register-pc! reg 2)

     (values `(stx-zero-page ,(bytes-ref code (+ 1 pc))) cpu)]
    [#x8c
     (step-register-pc! reg 3)

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'sty-abs addr) cpu)]
    [#x8d
     (step-register-pc! reg 3)

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'sta-abs addr) cpu)]
    [#x8e
     (step-register-pc! reg 3)

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'stx-abs addr) cpu)]
    [#x91
     (step-register-pc! reg 2)

     (values `(sta-indirect-y ,(bytes-ref code (+ 1 pc))) cpu)]
    [#x94
     (step-register-pc! reg 2)

     (values `(sty-zero-page-x ,(bytes-ref code (+ 1 pc))) cpu)]
    [#x95
     (step-register-pc! reg 2)

     (values `(sta-zero-page-x ,(bytes-ref code (+ 1 pc))) cpu)]
    [#x96
     (step-register-pc! reg 2)

     (values `(stx-zero-page-y ,(bytes-ref code (+ 1 pc))) cpu)]
    [#x99
     (step-register-pc! reg 3)

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'sta-abs-y addr) cpu)]
    [#x9d
     (step-register-pc! reg 3)

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'sta-abs-x addr) cpu)]
    [#xa0
     (step-register-pc! reg 2)

     (values `(ldy-imm ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xa1
     (step-register-pc! reg 2)

     (values `(lda-indirect-x ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xa2
     (step-register-pc! reg 2)

     (values `(ldx-imm ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xa4
     (step-register-pc! reg 2)

     (values `(ldy-zero-page ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xa5
     (step-register-pc! reg 2)

     (values `(lda-zero-page ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xa6
     (step-register-pc! reg 2)

     (values `(ldx-zero-page ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xa9
     (step-register-pc! reg 2)

     (values `(lda-imm ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xac
     (step-register-pc! reg 3)

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'ldy-abs addr) cpu)]
    [#xad
     (step-register-pc! reg 3)

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'lda-abs addr) cpu)]
    [#xae
     (step-register-pc! reg 3)

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'ldx-abs addr) cpu)]
    [#xb1
     (step-register-pc! reg 2)

     (values `(lda-indirect-y ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xb4
     (step-register-pc! reg 2)

     (values `(ldy-zero-page-x ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xb5
     (step-register-pc! reg 2)

     (values `(lda-zero-page-x ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xb6
     (step-register-pc! reg 2)

     (values `(ldx-zero-page-y ,(bytes-ref code (+ 1 pc))) cpu)]
    [#xb9
     (step-register-pc! reg 3)

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'lda-abs-y addr) cpu)]
    [#xbc
     (step-register-pc! reg 3)

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'ldy-abs-x addr) cpu)]
    [#xbd
     (step-register-pc! reg 3)

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'lda-abs-x addr) cpu)]
    [#xbe
     (step-register-pc! reg 3)

     (define addr (read-abs-addr-from code (+ 1 pc)))

     (values (list 'ldx-abs-y addr) cpu)]
    [_ (error 'invalid-op-code "invalid op code: #x~x" op-code)]))

(define/contract (decode op cpu)
  (-> any/c cpu? void)

  (define memory (cpu-memory cpu))

  (define reg (cpu-register cpu))

  (match op
    [(list 'lda-imm imm)
     (lda reg imm)

     (step-register-cycle! reg 2)

     cpu]
    [(list 'lda-zero-page addr)
     (define val (bytes-ref memory addr))

     (lda reg val)

     (step-register-cycle! reg 3)

     cpu]
    [(list 'lda-zero-page-x addr)
     (define x (register-x reg))

     (define val (bytes-ref memory (modulo (+ x addr) #xff)))

     (lda reg val)

     (step-register-cycle! reg 4)

     cpu]
    [(list 'lda-abs addr)
     (define val (bytes-ref memory addr))

     (lda reg val)

     (step-register-cycle! reg 4)

     cpu]
    [(list 'lda-abs-x base-addr)
     (define x (register-x reg))

     (define-values (addr crossed) (add-addr base-addr x))

     (define val (bytes-ref memory addr))

     (lda reg val)

     (step-register-cycle! reg (if crossed 5 4))

     cpu]
    [(list 'lda-abs-y base-addr)
     (define y (register-y reg))

     (define-values (addr crossed) (add-addr base-addr y))

     (define val (bytes-ref memory addr))

     (lda reg val)

     (step-register-cycle! reg (if crossed 5 4))

     cpu]
    [(list 'lda-indirect-x base-addr)
     (define x (register-x reg))

     (define-values (addr _) (add-addr base-addr x))

     (define memory-addr (read-abs-addr-from memory addr))

     (lda reg (bytes-ref memory memory-addr))

     (step-register-cycle! reg 6)

     cpu]
    [(list 'lda-indirect-y base-addr)
     (define y (register-y reg))

     (define memory-addr (read-abs-addr-from memory base-addr))

     (define-values (addr crossed) (add-addr memory-addr y))

     (lda reg (bytes-ref memory addr))

     (step-register-cycle! reg (if crossed 6 5))

     cpu]
    [(list 'ldx-imm imm)
     (ldx reg imm)

     (step-register-cycle! reg 2)

     cpu]
    [(list 'ldx-zero-page addr)
     (define val (bytes-ref memory addr))

     (ldx reg val)

     (step-register-cycle! reg 3)

     cpu]
    [(list 'ldx-zero-page-y addr)
     (define y (register-y reg))

     (define val (bytes-ref memory (modulo (+ y addr) #xff)))

     (ldx reg val)

     (step-register-cycle! reg 4)

     cpu]
    [(list 'ldx-abs addr)
     (define val (bytes-ref memory addr))

     (ldx reg val)

     (step-register-cycle! reg 4)

     cpu]
    [(list 'ldx-abs-y base-addr)
     (define y (register-y reg))

     (define-values (addr crossed) (add-addr base-addr y))

     (define val (bytes-ref memory addr))

     (ldx reg val)

     (step-register-cycle! reg (if crossed 5 4))

     cpu]
    [(list 'ldy-imm imm)
     (ldy reg imm)

     (step-register-cycle! reg 2)

     cpu]
    [(list 'ldy-zero-page addr)
     (define val (bytes-ref memory addr))

     (ldy reg val)

     (step-register-cycle! reg 3)

     cpu]
    [(list 'ldy-zero-page-x addr)
     (define x (register-x reg))

     (define val (bytes-ref memory (modulo (+ x addr) #xff)))

     (ldy reg val)

     (step-register-cycle! reg 4)

     cpu]
    [(list 'ldy-abs addr)
     (define val (bytes-ref memory addr))

     (ldy reg val)

     (step-register-cycle! reg 4)

     cpu]
    [(list 'ldy-abs-x base-addr)
     (define x (register-x reg))

     (define-values (addr crossed) (add-addr base-addr x))

     (define val (bytes-ref memory addr))

     (ldy reg val)

     (step-register-cycle! reg (if crossed 5 4))

     cpu]
    [(list 'sta-zero-page addr)
     (define a (register-a reg))

     (write-to-memory cpu addr (bytes a))

     (step-register-cycle! reg 3)

     cpu]
    [(list 'sta-zero-page-x addr)
     (define a (register-a reg))

     (define x (register-x reg))

     (define-values (zero-page-addr _) (add-addr-in-zero-page x addr))

     (write-to-memory cpu zero-page-addr (bytes a))

     (step-register-cycle! reg 4)

     cpu]
    [(list 'sta-abs addr)
     (define a (register-a reg))

     (write-to-memory cpu addr (bytes a))

     (step-register-cycle! reg 4)

     cpu]
    [(list 'sta-abs-x addr)
     (define a (register-a reg))

     (define x (register-x reg))

     (define-values (abs-addr _) (add-addr addr x))

     (write-to-memory cpu abs-addr (bytes a))

     (step-register-cycle! reg 4)

     cpu]
    [(list 'sta-abs-y addr)
     (define a (register-a reg))

     (define y (register-y reg))

     (define-values (abs-addr _) (add-addr addr y))

     (write-to-memory cpu abs-addr (bytes a))

     (step-register-cycle! reg 4)

     cpu]
    [(list 'sta-indirect-x addr)
     (define a (register-a reg))

     (define x (register-x reg))

     (define-values (indirect-addr _) (add-addr-in-zero-page addr x))

     (write-to-memory cpu (read-abs-addr-from memory indirect-addr) (bytes a))

     (step-register-cycle! reg 6)

     cpu]
    [(list 'sta-indirect-y addr)
     (define a (register-a reg))

     (define y (register-y reg))

     (define abs-addr (read-abs-addr-from memory addr))

     (define-values (added-addr _) (add-addr abs-addr y))

     (write-to-memory cpu added-addr (bytes a))

     (step-register-cycle! reg 6)

     cpu]
    [(list 'stx-zero-page addr)
     (define x (register-x reg))

     (write-to-memory cpu addr (bytes x))

     (step-register-cycle! reg 3)

     cpu]
    [(list 'stx-zero-page-y addr)
     (define x (register-x reg))

     (define y (register-y reg))

     (define-values (addr-in-zero-page _) (add-addr-in-zero-page addr y))

     (write-to-memory cpu addr-in-zero-page (bytes x))

     (step-register-cycle! reg 4)

     cpu]
    [(list 'stx-abs addr)
     (define x (register-x reg))

     (write-to-memory cpu addr (bytes x))

     (step-register-cycle! reg 4)

     cpu]
    [(list 'sty-zero-page addr)
     (define y (register-y reg))

     (write-to-memory cpu addr (bytes y))

     (step-register-cycle! reg 3)

     cpu]
    [(list 'sty-zero-page-x addr)
     (define x (register-x reg))

     (define y (register-y reg))

     (define-values (addr-in-zero-page _) (add-addr-in-zero-page addr x))

     (write-to-memory cpu addr-in-zero-page (bytes y))

     (step-register-cycle! reg 4)

     cpu]
    [(list 'sty-abs addr)
     (define y (register-y reg))

     (write-to-memory cpu addr (bytes y))

     (step-register-cycle! reg 4)

     cpu]
    [(list instruction _ ...) (error 'invalid-instruction "~a" instruction)]))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define (run1 code reg)
    (call-with-values (lambda ()
                        (define memory
                          (list->bytes (for/list ([i (in-range #x10000)])
                                         (modulo i #x100))))

                        (fetch code (cpu reg memory)))
                      decode))

  (define (lda)
    (test-suite "LDA"
      (test-suite "imm"
        (test-case "0xff"
          (define cpu (run1 #"\xa9\xff" (register 0 0 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #xff 0 0 2 0 0 1 2)))

        (test-case "0x00"
          (define cpu (run1 #"\xa9\x00" (register 0 0 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #x00 0 0 2 0 1 0 2))))

      (test-suite "zero page"
        (test-case "0x01"
          (define cpu (run1 #"\xa5\x01" (register 0 0 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #x01 0 0 2 0 0 0 3))))

      (test-suite "zero page x"
        (test-case "0x01"
          (define cpu (run1 #"\xb5\x01" (register 0 1 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #x02 1 0 2 0 0 0 4))))

      (test-suite "abs"
        (test-case "0x0001"
          (define cpu (run1 #"\xad\x00\x01" (register 0 0 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #x01 0 0 3 0 0 0 4))))

      (test-suite "abs x"
        (test-case "0x0001"
          (define cpu (run1 #"\xbd\x00\x01" (register 0 2 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #x03 2 0 3 0 0 0 4)))

        (test-case "page crossed"
          (define cpu (run1 #"\xbd\x00\xff" (register 0 2 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #x01 2 0 3 0 0 0 5))))

      (test-suite "abs y"
        (test-case "0x0001"
          (define cpu (run1 #"\xb9\x00\x01" (register 0 0 2 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #x03 0 2 3 0 0 0 4)))

        (test-case "page crossed"
          (define cpu (run1 #"\xb9\x00\xff" (register 0 0 2 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #x01 0 2 3 0 0 0 5))))

      (test-suite "indirect x"
        (test-case "0x01"
          (define cpu (run1 #"\xa1\x01" (register 0 2 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #x04 2 0 2 0 0 0 6))))

      (test-suite "indirect y"
        (test-case "0x01"
          (define cpu (run1 #"\xb1\x01" (register 0 0 3 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #x05 0 3 2 0 0 0 5))))))

  (define (ldx)
    (test-suite "LDX"
      (test-suite "imm"
        (test-case "0xff"
          (define cpu (run1 #"\xa2\xff" (register 0 0 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 #xff 0 2 0 0 1 2))))

      (test-suite "zero page"
        (test-case "0x01"
          (define cpu (run1 #"\xa6\x01" (register 0 0 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 #x01 0 2 0 0 0 3))))

      (test-suite "zero page y"
        (test-case "0x01"
          (define cpu (run1 #"\xb6\x01" (register 0 0 1 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 #x02 1 2 0 0 0 4))))

      (test-suite "abs"
        (test-case "0x0001"
          (define cpu (run1 #"\xae\x00\x01" (register 0 0 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 #x01 0 3 0 0 0 4))))

      (test-suite "abs y"
        (test-case "0x0001"
          (define cpu (run1 #"\xbe\x00\x01" (register 0 0 2 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 #x03 2 3 0 0 0 4)))

        (test-case "page crossed"
          (define cpu (run1 #"\xbe\x00\xff" (register 0 0 2 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 #x01 2 3 0 0 0 5))))))

  (define (ldy)
    (test-suite "LDY"
      (test-suite "imm"
        (test-case "0xff"
          (define cpu (run1 #"\xa0\xff" (register 0 0 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 0 #xff 2 0 0 1 2))))

      (test-suite "zero page"
        (test-case "0x01"
          (define cpu (run1 #"\xa4\x01" (register 0 0 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 0 #x01 2 0 0 0 3))))

      (test-suite "zero page x"
        (test-case "0x01"
          (define cpu (run1 #"\xb4\x01" (register 0 1 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 1 #x02 2 0 0 0 4))))

      (test-suite "abs"
        (test-case "0x0001"
          (define cpu (run1 #"\xac\x00\x01" (register 0 0 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 0 #x01 3 0 0 0 4))))

      (test-suite "abs x"
        (test-case "0x0001"
          (define cpu (run1 #"\xbc\x00\x01" (register 0 2 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 2 #x03 3 0 0 0 4))))

      (test-case "page crossed"
        (define cpu (run1 #"\xbc\x00\xff" (register 0 2 0 0 0 0 0 0)))

        (check-equal? (cpu-register cpu) (register 0 2 #x01 3 0 0 0 5)))))

  (define (sta)
    (test-suite "STA"
      (test-suite "zero page"
        (test-case "0x01"
          (define cpu (run1 #"\x85\x01" (register #xff 0 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #xff 0 0 2 0 0 0 3))

          (check-equal? (read-from-memory cpu #x01 1) #"\xff")))

      (test-suite "zero page x"
        (test-case "0x01"
          (define cpu (run1 #"\x95\x01" (register #xff 1 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #xff 1 0 2 0 0 0 4))

          (check-equal? (read-from-memory cpu #x02 1) #"\xff"))

        (test-case "overflow"
          (define cpu (run1 #"\x95\xff" (register #xff 1 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #xff 1 0 2 0 0 0 4))

          (check-equal? (read-from-memory cpu #x00 1) #"\xff")))

      (test-suite "abs"
        (test-case "0x0001"
          (define cpu (run1 #"\x8d\x00\x01" (register #xff 0 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #xff 0 0 3 0 0 0 4))

          (check-equal? (read-from-memory cpu #x0001 1) #"\xff")))

      (test-suite "abs x"
        (test-case "0x0001"
          (define cpu (run1 #"\x9d\x00\x01" (register #xff #x01 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #xff #x01 0 3 0 0 0 4))

          (check-equal? (read-from-memory cpu #x0002 1) #"\xff"))

        (test-case "overflow"
          (define cpu (run1 #"\x9d\xff\xff" (register #xff #x02 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #xff #x02 0 3 0 0 0 4))

          (check-equal? (read-from-memory cpu #x0001 1) #"\xff")))

      (test-suite "abs y"
        (test-case "0x0001"
          (define cpu (run1 #"\x99\x00\x01" (register #xff 0 #x01 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #xff 0 #x01 3 0 0 0 4))

          (check-equal? (read-from-memory cpu #x0002 1) #"\xff"))

        (test-case "overflow"
          (define cpu (run1 #"\x99\xff\xff" (register #xff 0 #x02 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #xff 0 #x02 3 0 0 0 4))

          (check-equal? (read-from-memory cpu #x0001 1) #"\xff")))

      (test-suite "indirect x"
        (test-case "0x01"
          (define cpu (run1 #"\x81\x01" (register #xff #x01 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #xff #x01 0 2 0 0 0 6))

          (check-equal? (read-from-memory cpu #x0203 1) #"\xff"))

        (test-case "overflow"
          (define cpu (run1 #"\x81\xff" (register #xff #x01 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #xff #x01 0 2 0 0 0 6))

          (check-equal? (read-from-memory cpu #x01 1) #"\xff")))

      (test-suite "indirect y"
        (test-case "0x01"
          (define cpu (run1 #"\x91\x01" (register #xff 0 #x01 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register #xff 0 #x01 2 0 0 0 6))

          (check-equal? (read-from-memory cpu #x0103 1) #"\xff"))

        (test-case "overflow"
          (define cpu (run1 #"\x91\xff" (register #xff 0 #xff 0 0 0 0 0)))

          (check-equal? 1 1)

          (check-equal? (cpu-register cpu) (register #xff 0 #xff 2 0 0 0 6))

          (check-equal? (read-from-memory cpu #xffff 1) #"\xff")))))

  (define (stx)
    (test-suite "STX"
      (test-suite "zero page"
        (test-case "0x01"
          (define cpu (run1 #"\x86\x01" (register 0 #xff 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 #xff 0 2 0 0 0 3))

          (check-equal? (read-from-memory cpu #x01 1) #"\xff")))

      (test-suite "zero page y"
        (test-case "0x01"
          (define cpu (run1 #"\x96\x01" (register 0 #xff 1 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 #xff 1 2 0 0 0 4))

          (check-equal? (read-from-memory cpu #x02 1) #"\xff"))

        (test-case "overflow"
          (define cpu (run1 #"\x96\xff" (register 0 #xff 1 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 #xff 1 2 0 0 0 4))

          (check-equal? (read-from-memory cpu #x00 1) #"\xff")))

      (test-suite "abs"
        (test-case "0x0001"
          (define cpu (run1 #"\x8e\x00\x01" (register 0 #xff 0 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 #xff 0 3 0 0 0 4))

          (check-equal? (read-from-memory cpu #x0001 1) #"\xff")))))

  (define (sty)
    (test-suite "STY"
      (test-suite "zero page"
        (test-case "0x01"
          (define cpu (run1 #"\x84\x01" (register 0 0 #xff 0 0 0 0 0)))

          (check-equal? (cpu-register cpu) (register 0 0 #xff 2 0 0 0 3))

          (check-equal? (read-from-memory cpu #x01 1) #"\xff"))))

    (test-suite "zero page x"
      (test-case "0x01"
        (define cpu (run1 #"\x94\x01" (register 0 1 #xff 0 0 0 0 0)))

        (check-equal? (cpu-register cpu) (register 0 1 #xff 2 0 0 0 4))

        (check-equal? (read-from-memory cpu #x02 1) #"\xff"))

      (test-case "overflow"
        (define cpu (run1 #"\x94\xff" (register 0 1 #xff 0 0 0 0 0)))

        (check-equal? (cpu-register cpu) (register 0 1 #xff 2 0 0 0 4))

        (check-equal? (read-from-memory cpu #x00 1) #"\xff")))

    (test-suite "abs"
      (test-case "0x0001"
        (define cpu (run1 #"\x8c\x00\x01" (register 0 0 #xff 0 0 0 0 0)))

        (check-equal? (cpu-register cpu) (register 0 0 #xff 3 0 0 0 4))

        (check-equal? (read-from-memory cpu #x0001 1) #"\xff"))))

  (run-tests (test-suite "run all"
               (lda)
               (ldx)
               (ldy)
               (sta)
               (stx)
               (sty))))
