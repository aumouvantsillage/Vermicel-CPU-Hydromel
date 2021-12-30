; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../asm/assembler.rkt"
  "../asm/opcodes.rkt"
  "../cpu/decoder.mel"
  hydromel/lib/signal
  hydromel/lib/slot
  hydromel/lib/helpers
  rackunit)

(define test-cases
  (list                         ; rd funct3          rs1  rs2       imm alu_fn     use_pc  use_imm has_rd is_load is_store is_jump is_branch is_mret
    ; Test rd, rs1, rs2 for R-type instruction.
    (list (ADD    0  0      0)     0 funct3_add_sub    0    0      'any '~alu_add  0       0       0      0       0        0       0         0)
    (list (ADD    5 10     15)     5 funct3_add_sub   10   15      'any '~alu_add  0       0       1      0       0        0       0         0)
    (list (ADD   31 31     31)    31 funct3_add_sub   31   31      'any '~alu_add  0       0       1      0       0        0       0         0)
    (list (ADD   40 41     42)     8 funct3_add_sub    9   10      'any '~alu_add  0       0       1      0       0        0       0         0)
    ; Test imm for I-type instruction (12 bits)
    (list (ADDI   5 10      0)     5 funct3_add_sub   10 'any         0 '~alu_add  0       1       1      0       0        0       0         0)
    (list (ADDI   5 10 #x07ff)     5 funct3_add_sub   10 'any    #x07ff '~alu_add  0       1       1      0       0        0       0         0)
    (list (ADDI   5 10 #x0800)     5 funct3_add_sub   10 'any   #x-0800 '~alu_add  0       1       1      0       0        0       0         0)
    (list (ADDI   5 10 #x0fff)     5 funct3_add_sub   10 'any        -1 '~alu_add  0       1       1      0       0        0       0         0)
    ; Test imm for S-type instruction (12 bits)
    (list (SW     5 10      0)  'any funct3_lw_sw     10    5         0 '~alu_add  0       1       0      0       1        0       0         0)
    (list (SW     5 10 #x07ff)  'any funct3_lw_sw     10    5    #x07ff '~alu_add  0       1       0      0       1        0       0         0)
    (list (SW     5 10 #x0800)  'any funct3_lw_sw     10    5   #x-0800 '~alu_add  0       1       0      0       1        0       0         0)
    (list (SW     5 10 #x0fff)  'any funct3_lw_sw     10    5        -1 '~alu_add  0       1       0      0       1        0       0         0)
    ; Test imm for B-type instruction (13 bits, even)
    (list (BEQ    5 10      0)  'any funct3_beq        5   10         0 '~alu_add  1       1       0      0       0        0       1         0)
    (list (BEQ    5 10 #x0fff)  'any funct3_beq        5   10    #x0ffe '~alu_add  1       1       0      0       0        0       1         0)
    (list (BEQ    5 10 #x1000)  'any funct3_beq        5   10   #x-1000 '~alu_add  1       1       0      0       0        0       1         0)
    (list (BEQ    5 10 #x1fff)  'any funct3_beq        5   10        -2 '~alu_add  1       1       0      0       0        0       1         0)
    ; Test imm for U-type instruction (32 bits, multiple of #x1000)
    (list (LUI    5    #x0000)     5 'any           'any 'any         0 '~alu_nop  0       1       1      0       0        0       0         0)
    (list (LUI    5    #x0fff)     5 'any           'any 'any         0 '~alu_nop  0       1       1      0       0        0       0         0)
    (list (LUI    5    #x1fff)     5 'any           'any 'any    #x1000 '~alu_nop  0       1       1      0       0        0       0         0)
    (list (LUI    5    #xffff)     5 'any           'any 'any    #xf000 '~alu_nop  0       1       1      0       0        0       0         0)
    (list (LUI    5        -1)     5 'any           'any 'any   #x-1000 '~alu_nop  0       1       1      0       0        0       0         0)
    ; Test imm for J-type instruction (21 bits, even)
    (list (JAL    5         0)     5 'any           'any 'any         0 '~alu_add  1       1       1      0       0        1       0         0)
    (list (JAL    5   #xfffff)     5 'any           'any 'any   #xffffe '~alu_add  1       1       1      0       0        1       0         0)
    (list (JAL    5  #x100000)     5 'any           'any 'any #x-100000 '~alu_add  1       1       1      0       0        1       0         0)
    (list (JAL    5  #x1fffff)     5 'any           'any 'any        -2 '~alu_add  1       1       1      0       0        1       0         0)
    ; Test functions for all instruction types.
    (list (LUI    0    #x1234)  'any 'any           'any 'any      'any '~alu_nop  0       1       0      0       0        0       0         0)
    (list (LUI   31    #x1234)  'any 'any           'any 'any      'any '~alu_nop  0       1       1      0       0        0       0         0)
    (list (AUIPC  0    #x1234)  'any 'any           'any 'any      'any '~alu_add  1       1       0      0       0        0       0         0)
    (list (AUIPC 31    #x1234)  'any 'any           'any 'any      'any '~alu_add  1       1       1      0       0        0       0         0)
    (list (JAL    0    #x1234)  'any 'any           'any 'any      'any '~alu_add  1       1       0      0       0        1       0         0)
    (list (JAL   31    #x1234)  'any 'any           'any 'any      'any '~alu_add  1       1       1      0       0        1       0         0)
    (list (JALR   0 15 #x1234)  'any funct3_jalr    'any 'any      'any '~alu_add  0       1       0      0       0        1       0         0)
    (list (JALR  31 15 #x1234)  'any funct3_jalr    'any 'any      'any '~alu_add  0       1       1      0       0        1       0         0)
    (list (BEQ    5 10 #x1234)  'any funct3_beq     'any 'any      'any '~alu_add  1       1       0      0       0        0       1         0)
    (list (BNE    5 10 #x1234)  'any funct3_bne     'any 'any      'any '~alu_add  1       1       0      0       0        0       1         0)
    (list (BLT    5 10 #x1234)  'any funct3_blt     'any 'any      'any '~alu_add  1       1       0      0       0        0       1         0)
    (list (BGE    5 10 #x1234)  'any funct3_bge     'any 'any      'any '~alu_add  1       1       0      0       0        0       1         0)
    (list (BLTU   5 10 #x1234)  'any funct3_bltu    'any 'any      'any '~alu_add  1       1       0      0       0        0       1         0)
    (list (BGEU   5 10 #x1234)  'any funct3_bgeu    'any 'any      'any '~alu_add  1       1       0      0       0        0       1         0)
    (list (LB     0 15 #x1234)  'any funct3_lb_sb   'any 'any      'any '~alu_add  0       1       0      1       0        0       0         0)
    (list (LB    31 15 #x1234)  'any funct3_lb_sb   'any 'any      'any '~alu_add  0       1       1      1       0        0       0         0)
    (list (LH     0 15 #x1234)  'any funct3_lh_sh   'any 'any      'any '~alu_add  0       1       0      1       0        0       0         0)
    (list (LH    31 15 #x1234)  'any funct3_lh_sh   'any 'any      'any '~alu_add  0       1       1      1       0        0       0         0)
    (list (LW     0 15 #x1234)  'any funct3_lw_sw   'any 'any      'any '~alu_add  0       1       0      1       0        0       0         0)
    (list (LW    31 15 #x1234)  'any funct3_lw_sw   'any 'any      'any '~alu_add  0       1       1      1       0        0       0         0)
    (list (LBU    0 15 #x1234)  'any funct3_lbu     'any 'any      'any '~alu_add  0       1       0      1       0        0       0         0)
    (list (LBU   31 15 #x1234)  'any funct3_lbu     'any 'any      'any '~alu_add  0       1       1      1       0        0       0         0)
    (list (LHU    0 15 #x1234)  'any funct3_lhu     'any 'any      'any '~alu_add  0       1       0      1       0        0       0         0)
    (list (LHU   31 15 #x1234)  'any funct3_lhu     'any 'any      'any '~alu_add  0       1       1      1       0        0       0         0)
    (list (SB     5 15 #x1234)  'any funct3_lb_sb   'any 'any      'any '~alu_add  0       1       0      0       1        0       0         0)
    (list (SH     5 15 #x1234)  'any funct3_lh_sh   'any 'any      'any '~alu_add  0       1       0      0       1        0       0         0)
    (list (SW     5 15 #x1234)  'any funct3_lw_sw   'any 'any      'any '~alu_add  0       1       0      0       1        0       0         0)
    (list (ADDI   0 10 #x1234)  'any funct3_add_sub 'any 'any      'any '~alu_add  0       1       0      0       0        0       0         0)
    (list (ADDI  31 10 #x1234)  'any funct3_add_sub 'any 'any      'any '~alu_add  0       1       1      0       0        0       0         0)
    (list (SLLI   0 10 #x1234)  'any funct3_sll     'any 'any      'any '~alu_sll  0       1       0      0       0        0       0         0)
    (list (SLLI  31 10 #x1234)  'any funct3_sll     'any 'any      'any '~alu_sll  0       1       1      0       0        0       0         0)
    (list (SLTI   0 10 #x1234)  'any funct3_slt     'any 'any      'any '~alu_slt  0       1       0      0       0        0       0         0)
    (list (SLTI  31 10 #x1234)  'any funct3_slt     'any 'any      'any '~alu_slt  0       1       1      0       0        0       0         0)
    (list (SLTIU  0 10 #x1234)  'any funct3_sltu    'any 'any      'any '~alu_sltu 0       1       0      0       0        0       0         0)
    (list (SLTIU 31 10 #x1234)  'any funct3_sltu    'any 'any      'any '~alu_sltu 0       1       1      0       0        0       0         0)
    (list (XORI   0 10 #x1234)  'any funct3_xor     'any 'any      'any '~alu_xor  0       1       0      0       0        0       0         0)
    (list (XORI  31 10 #x1234)  'any funct3_xor     'any 'any      'any '~alu_xor  0       1       1      0       0        0       0         0)
    (list (SRLI   0 10 #x1234)  'any funct3_srl_sra 'any 'any      'any '~alu_srl  0       1       0      0       0        0       0         0)
    (list (SRLI  31 10 #x1234)  'any funct3_srl_sra 'any 'any      'any '~alu_srl  0       1       1      0       0        0       0         0)
    (list (SRAI   0 10 #x1234)  'any funct3_srl_sra 'any 'any      'any '~alu_sra  0       1       0      0       0        0       0         0)
    (list (SRAI  31 10 #x1234)  'any funct3_srl_sra 'any 'any      'any '~alu_sra  0       1       1      0       0        0       0         0)
    (list (ORI    0 10 #x1234)  'any funct3_or      'any 'any      'any '~alu_or   0       1       0      0       0        0       0         0)
    (list (ORI   31 10 #x1234)  'any funct3_or      'any 'any      'any '~alu_or   0       1       1      0       0        0       0         0)
    (list (ANDI   0 10 #x1234)  'any funct3_and     'any 'any      'any '~alu_and  0       1       0      0       0        0       0         0)
    (list (ANDI  31 10 #x1234)  'any funct3_and     'any 'any      'any '~alu_and  0       1       1      0       0        0       0         0)
    (list (ADD    0  5     10)  'any funct3_add_sub 'any 'any      'any '~alu_add  0       0       0      0       0        0       0         0)
    (list (ADD   31  5     10)  'any funct3_add_sub 'any 'any      'any '~alu_add  0       0       1      0       0        0       0         0)
    (list (SUB    0  5     10)  'any funct3_add_sub 'any 'any      'any '~alu_sub  0       0       0      0       0        0       0         0)
    (list (SUB   31  5     10)  'any funct3_add_sub 'any 'any      'any '~alu_sub  0       0       1      0       0        0       0         0)
    (list (SLL    0  5     10)  'any funct3_sll     'any 'any      'any '~alu_sll  0       0       0      0       0        0       0         0)
    (list (SLL   31  5     10)  'any funct3_sll     'any 'any      'any '~alu_sll  0       0       1      0       0        0       0         0)
    (list (SLT    0  5     10)  'any funct3_slt     'any 'any      'any '~alu_slt  0       0       0      0       0        0       0         0)
    (list (SLT   31  5     10)  'any funct3_slt     'any 'any      'any '~alu_slt  0       0       1      0       0        0       0         0)
    (list (SLTU   0  5     10)  'any funct3_sltu    'any 'any      'any '~alu_sltu 0       0       0      0       0        0       0         0)
    (list (SLTU  31  5     10)  'any funct3_sltu    'any 'any      'any '~alu_sltu 0       0       1      0       0        0       0         0)
    (list (XOR    0  5     10)  'any funct3_xor     'any 'any      'any '~alu_xor  0       0       0      0       0        0       0         0)
    (list (XOR   31  5     10)  'any funct3_xor     'any 'any      'any '~alu_xor  0       0       1      0       0        0       0         0)
    (list (SRL    0  5     10)  'any funct3_srl_sra 'any 'any      'any '~alu_srl  0       0       0      0       0        0       0         0)
    (list (SRL   31  5     10)  'any funct3_srl_sra 'any 'any      'any '~alu_srl  0       0       1      0       0        0       0         0)
    (list (SRA    0  5     10)  'any funct3_srl_sra 'any 'any      'any '~alu_sra  0       0       0      0       0        0       0         0)
    (list (SRA   31  5     10)  'any funct3_srl_sra 'any 'any      'any '~alu_sra  0       0       1      0       0        0       0         0)
    (list (OR     0  5     10)  'any funct3_or      'any 'any      'any '~alu_or   0       0       0      0       0        0       0         0)
    (list (OR    31  5     10)  'any funct3_or      'any 'any      'any '~alu_or   0       0       1      0       0        0       0         0)
    (list (AND    0  5     10)  'any funct3_and     'any 'any      'any '~alu_and  0       0       0      0       0        0       0         0)
    (list (AND   31  5     10)  'any funct3_and     'any 'any      'any '~alu_and  0       0       1      0       0        0       0         0)))
    ;                             rd funct3          rs1  rs2       imm alu_fn     use_pc use_imm has_rd is_load is_store is_jump is_branch is_mret

(define test-count (length test-cases))

(define lst-data (map fake-asm (map first test-cases)))

(define inst (decoder-make))
(slot-set! (inst data) (list->signal lst-data))

(define field-names '(rd funct3 rs1 rs2 imm
                      alu_fn use_pc use_imm has_rd
                      is_load is_store is_jump is_branch is_mret))

(define lst-instr (signal-take (slot-ref inst instr) test-count))

(for ([(name i) (in-indexed field-names)])
  (for ([(instr j) (in-indexed lst-instr)])
    (define res (dict-ref instr name))
    (define ex (list-ref (list-ref test-cases j) (add1 i)))
    (when (not (equal? 'any ex))
      (test-equal? (format "Decoder #~a: ~a" j name) res ex))))
