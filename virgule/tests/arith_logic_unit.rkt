; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../cpu/common.mel"
  "../cpu/decoder.mel"
  "../cpu/arith_logic_unit.mel"
  "../asm/assembler.rkt"
  hydromel/lib/signal
  hydromel/lib/slot
  hydromel/lib/helpers
  rackunit)

(define test-cases
  (list
    (list (LUI  1 2)           10     20         20)
    (list (ADD  1 2 3)         10     20         30)
    (list (ADD  1 2 3)        -10    -20        -30)
    (list (SUB  1 2 3)         10     20        -10)
    (list (SUB  1 2 3)        -10    -20         10)
    (list (SLT  1 2 3)         10     20          1)
    (list (SLT  1 2 3)        -10     20          1)
    (list (SLT  1 2 3)         10    -20          0)
    (list (SLT  1 2 3)         10     10          0)
    (list (SLT  1 2 3)        -10    -10          0)
    (list (SLT  1 2 3)        -10    -20          0)
    (list (SLTU 1 2 3)         10     20          1)
    (list (SLTU 1 2 3)        -10     20          0)
    (list (SLTU 1 2 3)         10    -20          1)
    (list (SLTU 1 2 3)         10     10          0)
    (list (SLTU 1 2 3)        -10    -10          0)
    (list (SLTU 1 2 3)        -10    -20          0)
    (list (XOR  1 2 3)     #b0011 #b0101     #b0110)
    (list (OR   1 2 3)     #b0011 #b0101     #b0111)
    (list (AND  1 2 3)     #b0011 #b0101     #b0001)
    (list (SLL  1 2 3)    #x12345     12 #x12345000)
    (list (SRA  1 2 3)    #x12345     12       #x12)
    (list (SRA  1 2 3) #xF0005432     12 #xFFFF0005)
    (list (SRL  1 2 3)    #x12345     12       #x12)
    (list (SRL  1 2 3) #xF0005432     12 #x000F0005)))

(define test-count (length test-cases))

(define lst-data (map fake-asm      (map first  test-cases)))
(define lst-a    (map (word_t:impl) (map second test-cases)))
(define lst-b    (map (word_t:impl) (map third  test-cases)))
(define lst-x    (map (word_t:impl) (map fourth test-cases)))

(define dec-inst (decoder-make))
(slot-set! (dec-inst data) (list->signal lst-data))

(define alu-inst (arith_logic_unit-make))
(slot-set! (alu-inst instr) (slot-ref dec-inst instr))
(slot-set! (alu-inst a)     (list->signal lst-a))
(slot-set! (alu-inst b)     (list->signal lst-b))

(define lst-r (signal-take (slot-ref alu-inst r) test-count))

(for ([n (in-range test-count)]
      [a (in-list lst-a)]
      [b (in-list lst-b)]
      [r (in-list lst-r)]
      [x (in-list lst-x)])
  (test-equal? (format "ALU #~a: ~a ~a" n a b) r x))
