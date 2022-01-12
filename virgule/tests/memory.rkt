; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../devices/memory.mel"
  hydromel/lib/signal
  hydromel/lib/slot
  hydromel/lib/helpers
  (only-in data/collection length)
  data/pvector
  rackunit)

(define ram-init (pvector #x10203040
                          #x50607080
                          #x11213141
                          #x51617181
                          #x12223242
                          #x52627282
                          #x13233343
                          #x53637383))

(define test-cases-read
  (for/list ([(v n) (in-indexed ram-init)])
    ;     valid address wstrobe wdata      ready rdata
    (list 0     (* 4 n) #b0000  #x55555555 0     v)))

(define test-cases-write
  (list
    ;     valid address wstrobe wdata      ready rdata
    (list 0     0       #b0001  #x55667788 0     #x10203040)
    (list 1     0       #b0001  #x55667788 1     #x10203040)
    (list 0     0       #b0001  #x55667788 0     #x10203088)

    (list 0     1       #b0010  #x55667788 0     #x10203088)
    (list 1     1       #b0010  #x55667788 1     #x10203088)
    (list 0     1       #b0010  #x55667788 0     #x10207788)

    (list 0     2       #b0100  #x55667788 0     #x10207788)
    (list 1     2       #b0100  #x55667788 1     #x10207788)
    (list 0     2       #b0100  #x55667788 0     #x10667788)

    (list 0     3       #b1000  #x55667788 0     #x10667788)
    (list 1     3       #b1000  #x55667788 1     #x10667788)
    (list 0     3       #b1000  #x55667788 0     #x55667788)

    (list 0     16      #b0011  #x55667788 0     #x12223242)
    (list 1     16      #b0011  #x55667788 1     #x12223242)
    (list 0     16      #b0011  #x55667788 0     #x12227788)

    (list 0     18      #b1100  #x55667788 0     #x12227788)
    (list 1     18      #b1100  #x55667788 1     #x12227788)
    (list 0     18      #b1100  #x55667788 0     #x55667788)

    (list 0     24      #b1111  #x55667788 0     #x13233343)
    (list 1     24      #b1111  #x55667788 1     #x13233343)
    (list 0     24      #b1111  #x55667788 0     #x55667788)))

(define test-cases (append test-cases-read test-cases-write))

(define test-count (length test-cases))

(define ram-inst (single_port_ram-make (length ram-init) ram-init))
(slot-set! (ram-inst valid)   (list->signal (map first  test-cases)))
(slot-set! (ram-inst address) (list->signal (map second test-cases)))
(slot-set! (ram-inst wstrobe) (list->signal (map third  test-cases)))
(slot-set! (ram-inst wdata)   (list->signal (map fourth test-cases)))

(define lst-ready-expected (map fifth test-cases))
(define lst-rdata-expected (map sixth test-cases))

(define lst-ready (signal-take (slot-ref ram-inst ready) test-count))
(define lst-rdata (signal-take (slot-ref ram-inst rdata) test-count))

(for ([n       (in-range test-count)]
      [ready-x (in-list lst-ready-expected)]
      [rdata-x (in-list lst-rdata-expected)]
      [ready   (in-list lst-ready)]
      [rdata   (in-list lst-rdata)])
  (test-equal? (format "ready #~a" n) ready ready-x)
  (test-equal? (format "rdata #~a" n) rdata rdata-x))
