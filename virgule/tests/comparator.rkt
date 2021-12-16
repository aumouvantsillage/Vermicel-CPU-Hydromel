; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../cpu/decoder.mel"
  "../cpu/comparator.mel"
  "../asm/assembler.rkt"
  hydromel/lib/signal
  hydromel/lib/slot
  hydromel/lib/helpers
  rackunit)

(define test-cases
  (list
    (list (BEQ  1 2 3)  10  20 0)
    (list (BEQ  1 2 3)  10  10 1)
    (list (BEQ  1 2 3) -10 -20 0)
    (list (BEQ  1 2 3) -10 -10 1)
    (list (BNE  1 2 3)  10  20 1)
    (list (BNE  1 2 3)  10  10 0)
    (list (BNE  1 2 3) -10 -20 1)
    (list (BNE  1 2 3) -10 -10 0)
    (list (BLT  1 2 3)  10  20 1)
    (list (BLT  1 2 3) -10  20 1)
    (list (BLT  1 2 3)  10 -20 0)
    (list (BLT  1 2 3)  10  10 0)
    (list (BLT  1 2 3) -10 -10 0)
    (list (BLT  1 2 3) -10 -20 0)
    (list (BGE  1 2 3)  10  20 0)
    (list (BGE  1 2 3) -10  20 0)
    (list (BGE  1 2 3)  10 -20 1)
    (list (BGE  1 2 3)  10  10 1)
    (list (BGE  1 2 3) -10 -10 1)
    (list (BGE  1 2 3) -10 -20 1)
    (list (BLTU 1 2 3)  10  20 1)
    (list (BLTU 1 2 3) -10  20 0)
    (list (BLTU 1 2 3)  10 -20 1)
    (list (BLTU 1 2 3)  10  10 0)
    (list (BLTU 1 2 3) -10 -10 0)
    (list (BLTU 1 2 3) -10 -20 0)
    (list (BGEU 1 2 3)  10  20 0)
    (list (BGEU 1 2 3) -10  20 1)
    (list (BGEU 1 2 3)  10 -20 0)
    (list (BGEU 1 2 3)  10  10 1)
    (list (BGEU 1 2 3) -10 -10 1)
    (list (BGEU 1 2 3) -10 -20 1)))

(define test-count (length test-cases))

(define lst-data (map fake-asm      (map first  test-cases)))
(define lst-a    (map (word_t:impl) (map second test-cases)))
(define lst-b    (map (word_t:impl) (map third  test-cases)))
(define lst-x    (map (word_t:impl) (map fourth test-cases)))

(define dec-inst (decoder-make))
(slot-set! (dec-inst data) (list->signal lst-data))

(define cmp-inst (comparator-make))
(connect (slot-ref* dec-inst instr) (slot-ref* cmp-inst instr))
(slot-set! (cmp-inst a) (list->signal lst-a))
(slot-set! (cmp-inst b) (list->signal lst-b))

(define lst-r (signal-take (slot-ref cmp-inst taken) test-count))

(for ([n (in-range test-count)]
      [a (in-list lst-a)]
      [b (in-list lst-b)]
      [r (in-list lst-r)]
      [x (in-list lst-x)])
  (test-equal? (format "Comparator #~a: ~a ~a" n a b) r x))