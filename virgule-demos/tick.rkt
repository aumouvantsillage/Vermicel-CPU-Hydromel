; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  (except-in hydromel/sim zero)
  "../virgule/asm/assembler.rkt"
  "../virgule/devices/memory.rkt"
  "system.mel")

(define-values (tick-len tick-data)
  (asm->memory
    (J 'start)
    ; Interrupt service routine
    (LI   t0 tick_address) ; Acknowledge the IRQ.
    (LI   t1 1)
    (SB   t1 t0 4)
    (LI   t0 text_address)
    (ADDI t3 t3 1) ; Update and display the number of IRQs.
    (SW   t3 t0)
    (MRET)
    ; Main program
    'start
    (LI t3 0)
    (LI t0 tick_address)
    (LI t1 1)
    (SB t1 t0) ; Enable the tick device
    (J  0)))   ; Loop indefinitely

(define sys (system tick-len tick-data))

(define disp (signal-take (instance-ref sys '(text disp)) 500))
