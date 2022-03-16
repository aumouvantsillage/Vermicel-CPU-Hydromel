; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  hydromel/lib/helpers
  hydromel/lib/signal
  "../virgule/asm/assembler.rkt"
  "../virgule/devices/memory.rkt"
  (only-in "../virgule/asm/opcodes.rkt" hydromel-constants)
  "system.mel")

(hydromel-constants text_address)

(define-values (hello-len hello-data)
  (asm->memory
    (LI    t0 text_address)
    (LA    t1 'str) ; The address of the string
    'loop
    (LBU   t2 t1)   ; Read a character from the string
    (BEQZ  t2 0)    ; If zero, loop indefinitely
    (SB    t2 t0 4) ; Send the current character
    (ADDI  t1 t1 1) ; Move to the next location in the string
    (J     'loop)   ; Loop
    'str
    (bytes->words #:asciiz #t
      #"Virgule says\n<< Hello! >>\n")))

(define sys (system-make hello-len hello-data))

(define disp (signal-take (slot-ref sys text disp) 500))