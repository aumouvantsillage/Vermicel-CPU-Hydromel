; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  hydromel
  (for-syntax hydromel/lib/meta))

(provide
  show_int  show_int:impl  show_int:impl:return-type
  show_char show_char:impl show_char:impl:return-type)

(define-syntax show_int (make-function #'show_int:impl))

(define (show_int:impl n)
  (displayln n))

(define (show_int:impl:return-type tn)
  (none))

(define-syntax show_char (make-function #'show_char:impl))

(define (show_char:impl n)
  (display (integer->char n)))

(define (show_char:impl:return-type tn)
  (none))
