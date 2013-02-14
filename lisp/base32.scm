;;;
;;; Chicken Scheme implementation of Base32 decoding per RFC-4648
;;; *without* handling padding.
;;;
;;; Copyright (c) 2010-2013, Ross Lonstein
;;;
;;; Released under the BSD license, http://opensource.org/licenses/BSD-2-Clause
;;;

(declare (unit base32))

;; lazy programmer conveniences...
(define-syntax asr
  (syntax-rules ()
    ((_ i n)
     (arithmetic-shift i (- 0 n)))))

(define-syntax asl
  (syntax-rules ()
    ((_ i n)
     (arithmetic-shift i n))))


(define (base32-decode str)
  ;;
  (define b32-decoding-table
    '#(;0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
       -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ; 00-0F
       -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ; 10-1F
       -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ; 20-2F
       -1 -1 26 27 28 29 30 31 -1 -1 -1 -1 -1 -1 -1 -1 ; 30-3F (2-7)
       -1  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 ; 40-4F (A-N)
       15 16 17 18 19 20 21 22 23 24 25 ))             ; 50-5A (O-Z)

  (define (decode-valid-b32-char c)
    (if (and (char>? c #\nul) (char<? c #\[))
        (vector-ref b32-decoding-table (char->integer c))))

  (define (40bits->5bytes int)
    (list (integer->char (bitwise-and (asr int 32) #xff))
          (integer->char (bitwise-and (asr int 24) #xff))
          (integer->char (bitwise-and (asr int 16) #xff))
          (integer->char (bitwise-and (asr int 8)  #xff))
          (integer->char (bitwise-and int #xff))))

  (define (finalbits->bytes count int)
    ;;
    ;; After unpacking there might be remaining bits left in
    ;; the scratch space (bad packing/padding?).
    ;; Check the counter: for zero or one, silently drop them;
    ;; for 2-7, do our best to produce bytes.
    ;;
    (case count
      ((0 1) '())
      ((2) (list (bitwise-and (asr int 2) #xff)))
      ((3) (list (bitwise-and (asr int 7) #xff)))
      ((4) (set! int (asr int 4))
           (list (bitwise-and (asr int 8) #xff)
                 (bitwise-and int #xff)))
      ((5) (set! int (asr int 1))
           (list (bitwise-and (asr int 16) #xff)
                 (bitwise-and (asr int 8) #xff)
                 (bitwise-and int #xff)))
      ((6) (set! int (asr int 6))
           (list (bitwise-and (asr int 16) #xff)
                 (bitwise-and (asr int 8) #xff)
                 (bitwise-and int #xff)))
      ((7) (set! int (asr int 3))
           (list (bitwise-and (asr int 24) #xff)
                 (bitwise-and (asr int 16) #xff)
                 (bitwise-and (asr int 8) #xff)
                 (bitwise-and int #xff)))))

  ;;
  ;;TODO: for efficiency presize the string and set! the bytes in it
  ;; instead of fiddling with lists and conversions, ex.
  ;; (let (decoded (make-string (/ (+ 7 (string-length k)) 40)))
  ;;   ...
  ;;   (string-set! decoded decoded-idx (...))
  ;;  )
  ;;    ...
  ;;

  (do ((idx 0 (add1 idx))
          (decoded '())
          (strlen (sub1 (string-length str)))
          (counter 0)
          (scratch 0))
      ((< strlen idx) (list->string
                       (concatenate
                        (list decoded (finalbits->bytes counter scratch)))))
    (let ((c (decode-valid-b32-char (string-ref str idx))))
      (when (and (not (null? c)) (> c 0))
        (set! scratch (+ (asl scratch 5) c))
        (set! counter (modulo (add1 counter) 8))
        (when (zero? counter)
          (set! decoded (append decoded (40bits->5bytes scratch)))
          (set! scratch 0)))))) ; base32-decode
