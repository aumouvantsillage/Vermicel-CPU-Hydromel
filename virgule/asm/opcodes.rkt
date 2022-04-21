; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../cpu/opcodes.mel"
  hydromel/lib/numeric
  hydromel/lib/slot
  syntax/parse/define
  (for-syntax racket/syntax))

(provide (all-defined-out))

(define (instruction-fmt opcode)
  (match opcode
    [(== opcode_op)                         'fmt-r]
    [(== opcode_store)                      'fmt-s]
    [(== opcode_branch)                     'fmt-b]
    [(or (== opcode_lui) (== opcode_auipc)) 'fmt-u]
    [(== opcode_jal)                        'fmt-j]
    [_                                      'fmt-i]))

(define (word->fields w)
  (define opcode (unsigned-slice w 6 0))
  (define imm (match (instruction-fmt opcode)
                ['fmt-i (signed-concat* [w 31 20])]
                ['fmt-s (signed-concat* [w 31 25] [w 11  7])]
                ['fmt-b (signed-concat* [w 31 31] [w  7  7] [w 30 25] [w 11  8] [0 0 0])]
                ['fmt-u (signed-concat* [w 31 12] [0 11  0])]
                ['fmt-j (signed-concat* [w 31 31] [w 19 12] [w 20 20] [w 30 21] [0 0 0])]
                [_      0]))
  (values opcode
          (unsigned-slice w 11  7) ; rd
          (unsigned-slice w 14 12) ; funct3
          (unsigned-slice w 19 15) ; rs1
          (unsigned-slice w 24 20) ; rs2
          (unsigned-slice w 31 25) ; funct7
          imm))

(define (fields->word opcode #:rd [rd 0] #:funct3 [fn3 0] #:rs1 [rs1 0] #:rs2 [rs2 0] #:funct7 [fn7 0] #:imm [imm 0])
  ; Immediate shift instructions have a funct7 field and a shorter imm field.
  (define imm* (if (and (= opcode opcode_op_imm)
                        (or (= fn3 funct3_sll)
                            (= fn3 funct3_srl_sra)))
                 (unsigned-concat* [fn7 6 0] [imm 4 0])
                 imm))

  (match (instruction-fmt opcode)
    ;                               31          30 25      24 21       20          19 15      14 12      11 8           7          6 0
    ['fmt-i (unsigned-concat* [imm* 11                                  0]    [rs1  4  0] [fn3 2  0] [rd  4             0] [opcode 6 0])]
    ['fmt-s (unsigned-concat* [imm  11              5] [rs2 4           0]    [rs1  4  0] [fn3 2  0] [imm 4             0] [opcode 6 0])]
    ['fmt-b (unsigned-concat* [imm  12 12] [imm 10  5] [rs2 4           0]    [rs1  4  0] [fn3 2  0] [imm 4 1] [imm 11 11] [opcode 6 0])]
    ['fmt-u (unsigned-concat* [imm  31                                                           12] [rd  4             0] [opcode 6 0])]
    ['fmt-j (unsigned-concat* [imm  20 20] [imm 10             1] [imm 11 11] [imm 19            12] [rd  4             0] [opcode 6 0])]
    [_      (unsigned-concat* [fn7   6              0] [rs2 4           0]    [rs1 4   0] [fn3 2  0] [rd  4             0] [opcode 6 0])]))
