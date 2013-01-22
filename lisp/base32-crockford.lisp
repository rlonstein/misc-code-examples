;;;
;;; Common Lisp implementation of Douglas Crockford's Base32 encoding
;;; (http://www.crockford.com/wrmg/base32.html)
;;;
;;; Copyright (c) 2010, Ross Lonstein
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

(cl:defpackage #:b32c
    (:use  #:cl)
  (:export #:b32c-encode #:b32c-decode))

(in-package #:b32c)

(defvar encoding-table #.(coerce "0123456789ABCDEFGHJKMNPQRSTVWXYZ*~$=U" 'simple-string))

(defvar decoding-table
  (make-array 36 :element-type 'integer
              :adjustable nil
              :fill-pointer nil
              :displaced-to nil
              :initial-contents '(0 1 2 3 4 5 6 7 8 9      ; numbers 0-9
                                  10 11 12 13 14 15 16 17  ; A,B,C,D,E,F,G,H
                                  1 18 19 1 20 21 0 22     ; I,J,K,L,M,N,O,P
                                  23 24 25 26              ; Q,R,S,T
                                  36                       ; U (checksum, others handled specially)
                                  27 28 29 30 31           ; V,W,X,Y,Z
                                  )))

(defun char2value (c)
  (declare (type standard-char c))
  (let ((chr (char-upcase c)))
    (cond ((digit-char-p chr) (digit-char-p chr))
          ((alpha-char-p chr) (aref decoding-table (digit-char-p chr 36)))
          ((char= #\* chr) 32) ; checksum digit
          ((char= #\~ chr) 33) ; checksum digit
          ((char= #\$ chr) 34) ; checksum digit
          ((char= #\= chr) 35) ; checksum digit
          (t (error "Illegal char '~C'" c)))))

(defun value2char (i)
  (declare (type (integer 0 37) i))
  (schar encoding-table i))

(defun normalize-string (str)
  (declare (type simple-string str))
  (remove-if #'(lambda (c) (or (char= #\- c) (char= #\Space c))) str))

(defun checksum-chr (int)
  (declare (type integer int))
  (value2char (mod int 37)))

(defun b32-encoded-length (int)
  (multiple-value-bind (q r) (ceiling (integer-length int) 5)
    (+ (if (zerop q) 1 q) (if (plusp r) 1 0))))

(defun b32c-encode (int &key checksum)
  (loop :with len = (b32-encoded-length int) ; encode into a preallocated string
        :with output = (make-string len :element-type 'standard-char :initial-element #\Space)
        :for n = int :then q
        :for q = (ash n -5)      ; lshift faster than (floor n 32), works but is it valid?
        :for r = (- n (ash q 5)) ; use quotient to find remainder (r = q * 2^k - n) from H.S. Warren, Jr.
        :for idx :downfrom (1- len)
        :do (setf (char output idx) (value2char (if (zerop q) n r)))
        :until (zerop q)
        :finally (return (concatenate 'simple-string output (when checksum (string (checksum-chr int)))))))

(defun b32c-decode (str &key checksum)
  (let ((normalized-str (normalize-string str))
        (checkchr nil))
    (when checksum
      (let ((l (1- (length normalized-str))))
        (setf checkchr (char normalized-str l))
        (setf normalized-str (subseq str 0 l))))
    ;; Slightly clever. Instead of doing bit manipulations use
    ;; positional notation. V = D * B^P where digit's value (V) is the
    ;; digit (D) multiplied by the exponentiation of the Base (B) and
    ;; the positional value (P).  Ex: 465 in base10 is
    ;;   4*10^2 + 6*10^1 + 5*10^0 = 4*100 + 6*10 + 5*1 = 465
    ;; http://en.wikipedia.org/wiki/Positional_notation
    (values (loop :for c :across (nreverse normalized-str)
                  :for offset = 1 :then (* offset 32)
                  :summing (* (char2value c) offset))
            (if checksum (char2value checkchr)))))
