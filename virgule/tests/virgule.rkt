; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../asm/assembler.rkt"
  "../cpu/virgule.mel"
  hydromel/lib/signal
  hydromel/lib/helpers
  rackunit)

(define test-cases
  (list
    ;     rdata            ready irq valid address    wstrobe wdata          state
    (list 0                0     0   1     #x00000000 #b0000  'any)          ; F
    (list (LUI 4 #xA000)   1     0   1     #x00000000 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                0     0   0     'any       'any    'any)          ; R

    ; At this point, x4 = #x0000A000

    (list (ADDI 5 0 #x96)  1     0   1     #x00000004 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                0     0   0     'any       'any    'any)          ; R

    ; At this point, x5 = #x00000096

    (list (SW   5 4 #x100) 1     0   1     #x00000008 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                0     0   1     #x0000A100 #b1111  #x00000096)    ; S
    (list 0                1     0   1     #x0000A100 #b1111  #x00000096)    ; S

    (list (SH   5 4 #x100) 1     0   1     #x0000000C #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A100 #b0011  #x00960096)    ; S

    (list (SH   5 4 #x102) 1     0   1     #x00000010 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A102 #b1100  #x00960096)    ; S

    (list (SB   5 4 #x100) 1     0   1     #x00000014 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A100 #b0001  #x96969696)    ; S

    (list (SB   5 4 #x101) 1     0   1     #x00000018 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A101 #b0010  #x96969696)    ; S

    (list (SB   5 4 #x102) 1     0   1     #x0000001C #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A102 #b0100  #x96969696)    ; S

    (list (SB   5 4 #x103) 1     0   1     #x00000020 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A103 #b1000  #x96969696)    ; S

    (list (LW   6 4 #x100) 1     0   1     #x00000024 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                0     0   1     #x0000A100 #b0000  'any)          ; L
    (list #x8C15F3E4       1     0   1     #x0000A100 #b0000  'any)          ; L
    (list 0                0     0   0     'any       'any    'any)          ; R

    ; At this point, x6 = #x8C15F3E4

    (list (SW   6 4 #x100) 1     0   1     #x00000028 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A100 #b1111  #x8C15F3E4)    ; S

    (list (LH   7 4 #x100) 1     0   1     #x0000002C #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list #x8C15F3E4       1     0   1     #x0000A100 #b0000  'any)          ; L
    (list 0                0     0   0     'any       'any    'any)          ; R

    ; At this point, x7 = #xFFFFF3E4

    (list (SW   7 4 #x100) 1     0   1     #x00000030 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A100 #b1111  #xFFFFF3E4)    ; S

    (list (LH   8 4 #x102) 1     0   1     #x00000034 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list #x8C15F3E4       1     0   1     #x0000A102 #b0000  'any)          ; L
    (list 0                0     0   0     'any       'any    'any)          ; R

    ; At this point, x8 = #xFFFF8C15

    (list (SW   8 4 #x100) 1     0   1     #x00000038 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A100 #b1111  #xFFFF8C15)    ; S

    (list (LHU  9 4 #x100) 1     0   1     #x0000003C #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list #x8C15F3E4       1     0   1     #x0000A100 #b0000  'any)          ; L
    (list 0                0     0   0     'any       'any    'any)          ; R

    ; At this point, x9 = #x0000F3E4

    (list (SW   9 4 #x100) 1     0   1     #x00000040 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A100 #b1111  #x0000F3E4)    ; S

    (list (LHU 10 4 #x102) 1     0   1     #x00000044 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list #x8C15F3E4       1     0   1     #x0000A102 #b0000  'any)          ; L
    (list 0                0     0   0     'any       'any    'any)          ; R

    ; At this point, x10 = #x00008C15

    (list (SW  10 4 #x100) 1     0   1     #x00000048 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A100 #b1111  #x00008C15)  ; S

    (list (LB  11 4 #x100) 1     0   1     #x0000004C #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list #x8C15F3E4       1     0   1     #x0000A100 #b0000  'any)          ; L
    (list 0                0     0   0     'any       'any    'any)          ; R

    ; At this point, x11 = #xFFFFFFE4

    (list (SW  11 4 #x100) 1     0   1     #x00000050 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A100 #b1111  #xFFFFFFE4)    ; S

    (list (LB  12 4 #x101) 1     0   1     #x00000054 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list #x8C15F3E4       1     0   1     #x0000A101 #b0000  'any)          ; L
    (list 0                0     0   0     'any       'any    'any)          ; R

    ; At this point, x12 = #xFFFFFFF3

    (list (SW  12 4 #x100) 1     0   1     #x00000058 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A100 #b1111  #xFFFFFFF3)    ; S

    (list (LB  13 4 #x102) 1     0   1     #x0000005C #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list #x8C15F3E4       1     0   1     #x0000A102 #b0000  'any)          ; L
    (list 0                0     0   0     'any       'any    'any)          ; R

    ; At this point, x13 = #x00000015

    (list (SW  13 4 #x100) 1     0   1     #x00000060 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A100 #b1111  #x00000015)    ; S

    (list (LB  14 4 #x103) 1     0   1     #x00000064 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list #x8C15F3E4       1     0   1     #x0000A103 #b0000  'any)          ; L
    (list 0                0     0   0     'any       'any    'any)          ; R

    ; At this point, x14 = #xFFFFFF8C

    (list (SW  14 4 #x100) 1     0   1     #x00000068 #b0000  'any)          ; F
    (list 0                0     0   0     'any       'any    'any)          ; D
    (list 0                0     0   0     'any       'any    'any)          ; E
    (list 0                1     0   1     #x0000A100 #b1111  #xFFFFFF8C)))  ; S

(define test-count (length test-cases))

(define lst-rdata    (map fake-asm (map first  test-cases)))
(define lst-ready                  (map second test-cases))
(define lst-irq                    (map third  test-cases))
(define lst-expected (for/list ([c (in-list test-cases)])
                       (drop c 3)))

(define cpu-inst (virgule-make))
(slot-set! (cpu-inst reset) (signal 0))
(slot-set! (cpu-inst rdata) (list->signal lst-rdata))
(slot-set! (cpu-inst ready) (list->signal lst-ready))
(slot-set! (cpu-inst irq)   (list->signal lst-irq))

(define lst-result (map list
                     (signal-take (slot-ref cpu-inst valid)   test-count)
                     (signal-take (slot-ref cpu-inst address) test-count)
                     (signal-take (slot-ref cpu-inst wstrobe) test-count)
                     (signal-take (slot-ref cpu-inst wdata)   test-count)))

(define labels '(valid address wstrobe wdata))

(for ([n (in-range test-count)]
      [r (in-list lst-result)]
      [x (in-list lst-expected)])
  (for ([l  (in-list labels)]
        [rv (in-list r)]
        [xv (in-list x)]
        #:when (not (equal? 'any xv)))
    (test-equal? (format "Virgule #~a: ~a" n l) rv xv)))
