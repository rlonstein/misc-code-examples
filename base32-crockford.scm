;;;
;;; Chicken Scheme implementation of Douglas Crockford's Base32 encoding
;;; (http://www.crockford.com/wrmg/base32.html) translated from my own
;;; earlier implementation in Common Lisp
;;;
;;; Copyright (c) 2010-2011, Ross Lonstein
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.
;;;

(define encoding-table '#( #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                           #\A #\B #\C #\D #\E #\F #\G #\H #\J #\K
                           #\M #\N #\P #\Q #\R #\S #\T #\V #\W #\X
                           #\Y #\Z #\* #\- #\% #\= #\U ))

(define decoding-table '#( 0  1  2  3  4  5  6  7  8  9 ; numbers 0-9
                          10 11 12 13 14 15 16 17       ; A-H
                           1 18 19  1 20 21  0 22       ; I,J,K,L,M,N,O,P
                          23 24 25 26                   ; Q,R,S,T
                          36                            ; U (checksum)
                          27 28 29 30 31 ))             ; V,W,X,Y,Z

;;
;; Assumptions about ASCII ordering of chars here...
;;
(define-constant char-num-base (char->integer #\0))
(define-constant char-alpha-base (char->integer #\A))

(define (numchar->value chr)
  ;; don't bother with lookup, just calculate value
  (if (char-numeric? chr) (- (char->integer chr) char-num-base)
      (abort (make-property-condition
                          'char-not-numeric
                          'value
                          chr))))

(define (alphachar->value chr)
  (if (char-alphabetic? chr)
      (vector-ref decoding-table 
                  (+ 10 (- (char->integer (char-upcase chr)) char-alpha-base)))
      (abort (make-property-condition
              'char-not-alpha
              'value
              chr))))

(define (char->value chr)
  (cond
   ((char-numeric? chr) (numchar->value chr))
   ((char-alphabetic? chr) (alphachar->value chr))
   ((char=? chr #\*) 32)
   ((char=? chr #\-) 33)
   ((char=? chr #\$) 34)
   ((char=? chr #\=) 35)
   (else (abort (make-property-condition
                 'invalid-char
                 'value
                 chr)))))

(define (value->char int)
  (vector-ref encoding-table int))

(define (b32c-checksum int)
  (modulo int 37))

(define (b32c-checksum-char int)
  (value->char (b32c-checksum int)))

(define (b32c-valid-checksum? int checksum)
  (= checksum (b32c-checksum int)))

(define (exact-integer? n)
  (and (integer? n) (exact? n)))


;;;;
;;;; floor & ceiling functions from MIT Scheme by Taylor R. Campbell
;;;;

(define (ceiling-/- n d)
  (let ((n (- 0 n)) (d (- 0 d)))
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values q r)
          (values (+ q 1) (- d r))))))

(define (ceiling+/+ n d)
  (let ((q (quotient n d)) (r (remainder n d)))
    (if (zero? r)
        (values q r)
        (values (+ q 1) (- r d)))))

(define (ceiling/ n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (ceiling-/- n d))
            ((negative? n)
             (let ((n (- 0 n)))
               (values (- 0 (quotient n d)) (- 0 (remainder n d)))))
            ((negative? d)
             (let ((d (- 0 d)))
               (values (- 0 (quotient n d)) (remainder n d))))
            (else
             (ceiling+/+ n d)))
      (let ((q (ceiling (/ n d))))
        (values q (- n (* d q))))))

(define (floor-/+ n d)
  (let ((n (- 0 n)))
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values (- 0 q) r)
          (values (- (- 0 q) 1) (- d r))))))

(define (floor+/- n d)
  (let ((d (- 0 d)))
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values (- 0 q) r)
          (values (- (- 0 q) 1) (- r d))))))

(define (floor/ n d)
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (let ((n (- 0 n)) (d (- 0 d)))
               (values (quotient n d) (- 0 (remainder n d)))))
            ((negative? n) (floor-/+ n d))
            ((negative? d) (floor+/- n d))
            (else (values (quotient n d) (remainder n d))))
      (let ((q (floor (/ n d))))
        (values q (- n (* d q))))))
;;;;

(define (logn n base)
  (/ (log n) (log base)))

(define (integer-length int)
  (ceiling (logn (if (negative? int)
                     (- int)
                     (+ 1 int))
                 2)))

(define (b32-encoded-length int)
  (let-values (((q r) (ceiling/ (integer-length int) 5)))
    (+ (if (zero? q) 1 q) (if (positive? r) 1 0))))

(define (b32c-encode int #!key checksum)
  (let loop ((n int)
             (acc '()))
    (if (zero? n) (list->string (if checksum
                                    (append acc (list (b32c-checksum-char int)))
                                    acc))
        (let-values (((q r) (floor/ n 32)))
          (loop q (cons (value->char (if (zero? q) n r)) acc))))))


(define (normalize-b32-string str)
  (string-delete (char-set #\- #\space) str))

(define (b32c-decode str #!key checksum)
  (let ((normalized-str (normalize-b32-string str))
        (checkchar #f))
    (when checksum
          (let ((len (string-length str)))
            (set! checkchar (string-ref str (sub1 len)))
            (set! str (substring str 0 (sub1 len)))))
    (let loop ((c (reverse (string->list str)))
               (offset 1)
               (sum 0))
      (values (if (null? c) sum
                  (loop (cdr c)
                        (* 32 offset)
                        (+ sum (* (char->value (car c)) offset))))
              (if checksum (char->value checkchar))))))
