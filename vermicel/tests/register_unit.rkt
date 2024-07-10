; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  (except-in hydromel/support zero)
  "../cpu/common.mel"
  "../cpu/decoder.mel"
  "../cpu/register_unit.mel"
  "../asm/assembler.rkt")

(define (write-value n)
  (* #x1000 (add1 n)))

(define (read-value n)
  (if (zero? n)
    0
    (write-value n)))

(define (write-test-case rd)
  ;     src-instr       dest-instr   enable xd               xs1              xs2
  (list (ADD 0 rd  rd)  (ADD rd 0 0) 1      (write-value rd) 0                0))

(define (read-test-case rs1 rs2)
  ;     src-instr       dest-instr   enable xd               xs1              xs2
  (list (ADD 0 rs1 rs2) (ADD 0  0 0) 0      0                (read-value rs1) (read-value rs2)))

(define write-test-cases
  (for/list ([n 32])
    (write-test-case n)))

(define read-test-cases
  (apply append
    (for/list ([n 32])
      (list
        (read-test-case n 0)
        (read-test-case 0 n)
        (read-test-case n (- 31 n))))))

(define test-cases
  (append write-test-cases read-test-cases))

(define test-count (length test-cases))

(define lst-src-data     (map fake-asm      (map first  test-cases)))
(define lst-dest-data    (map fake-asm      (map second test-cases)))
(define lst-enable                          (map third  test-cases))
(define lst-xd           (map (word_t) (map fourth test-cases)))
(define lst-expected-xs1 (map (word_t) (map fifth  test-cases)))
(define lst-expected-xs2 (map (word_t) (map sixth  test-cases)))

(define dec-src-inst (decoder))
(instance-set! dec-src-inst 'data (list->signal lst-src-data))

(define dec-dest-inst (decoder))
(instance-set! dec-dest-inst 'data (list->signal lst-dest-data))

(define reg-inst (register_unit 32))
(instance-set! reg-inst 'src_instr  (instance-ref dec-src-inst  'instr))
(instance-set! reg-inst 'dest_instr (instance-ref dec-dest-inst 'instr))
(instance-set! reg-inst 'reset      (signal 0))
(instance-set! reg-inst 'enable     (list->signal lst-enable))
(instance-set! reg-inst 'xd         (list->signal lst-xd))

(define lst-xs1 (signal-take (instance-ref reg-inst 'xs1) test-count))
(define lst-xs2 (signal-take (instance-ref reg-inst 'xs2) test-count))

(for ([n  test-count]
      [si (in-list lst-src-data)]
      [di (in-list lst-dest-data)]
      [r1 (in-list lst-xs1)]
      [r2 (in-list lst-xs2)]
      [x1 (in-list lst-expected-xs1)]
      [x2 (in-list lst-expected-xs2)])
  (test-equal? (format "Register unit #~a: rs1" n)
    r1 x1)
  (test-equal? (format "Register unit #~a: rs2" n)
    r2 x2))