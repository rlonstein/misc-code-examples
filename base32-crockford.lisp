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

(defvar encoding-table
  (make-array 37 :element-type 'character
              :adjustable nil
              :initial-contents '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                                  #\A #\B #\C #\D #\E #\F #\G #\H
                                  #\J #\K
                                  #\M #\N
                                  #\P #\Q #\R #\S #\T
                                  #\V #\W #\X #\Y #\Z
                                  #\* #\~ #\$ #\= #\U)))

(defvar decoding-table
  (make-array 36 :element-type 'integer
              :adjustable nil
              :initial-contents '(0 1 2 3 4 5 6 7 8 9      ; numbers 0-9
                                  10 11 12 13 14 15 16 17  ; A,B,C,D,E,F,G,H
                                  1 18 19 1 20 21 0 22     ; I,J,K,L,M,N,O,P
                                  23 24 25 26              ; Q,R,S,T
                                  36                       ; U (checksum, others handled specially)
                                  27 28 29 30 31           ; V,W,X,Y,Z
                                  )))

(defun char2value (c)
  (let ((chr (char-upcase c)))
    (cond ((digit-char-p chr) (digit-char-p chr))
          ((alpha-char-p chr) (aref decoding-table (digit-char-p chr 36)))
          ((char= #\* chr) 32) ; checksum digit
          ((char= #\~ chr) 33) ; checksum digit
          ((char= #\$ chr) 34) ; checksum digit
          ((char= #\= chr) 35) ; checksum digit
          (t (error "Illegal char '~C'" c)))))

(defun value2char (i)
  (aref encoding-table i))

(defun normalize-string (str)
  (remove-if #'(lambda (c) (or (char= #\- c) (char= #\Space c))) str))

(defun b32c-encode (int &key checksum)
  ;;FIXME: this can be tightened up
  ;;TODO: implement chunking
  (loop :for n = int :then q
        :for q = (ash n -5) ; bumming- lshift faster than mod, works but is valid?
        :for r = (rem n 32)
        :collect (value2char (if (zerop q) n r)) :into digits
        :until (zerop q)
        :finally (return (concatenate 'string (nreverse digits) (when checksum (string (checksum-chr int)))))))

(defun checksum-chr (int)
  (value2char (mod int 37)))

(defun b32c-decode (str &key checksum)
  (let ((normalized-str (normalize-string str))
        (checkchr nil)
        (total 0))
    (when checksum
      (setf checkchr (char normalized-str (1- (length normalized-str))))
      (setf normalized-str (subseq str 0 (1- (length normalized-str)))))
    ;; Slightly clever. Instead of doing bit manipulations use
    ;; positional notation. V = D * B^P where digit's value (V) is the
    ;; digit (D) multiplied by the exponentiation of the Base (B) and
    ;; the positional value (P).  Ex: 465 in base10 is
    ;;   4*10^2 + 6*10^1 + 5*10^0 = 4*100 + 6*10 + 5*1 = 465
    ;; http://en.wikipedia.org/wiki/Positional_notation
    (setf total (loop :for c :across (nreverse normalized-str)
                      :for offset = 1 :then (* offset 32)
                      :summing (* (char2value c) offset)))
    (values total (if checksum (char2value checkchr)))))
