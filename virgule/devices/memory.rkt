; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../asm/assembler.rkt"
  data/pvector
  data/collection
  threading)

(provide asm->memory)

; Assemble a list of instructions using asm.
; Return the result as a pvector whose length is a power of 2.
(define (asm->memory . instrs)
  (define bin (apply asm instrs))
  (define total-length (~>> bin length sub1 integer-length (arithmetic-shift 1)))
  (values
    total-length
    (extend* (pvector) bin (make-pvector (- total-length (length bin)) 0))))
