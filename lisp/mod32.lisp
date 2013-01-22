;;;
;;; various 32-bit operations needed for doing C to Lisp translations
;;;
;;; Copyright (c) 2008, Ross Lonstein
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

(defun mod32+ (a b)
  "32-bit addition"
  (declare (type (unsigned-byte 32) a b))
  (ldb (byte 32 0) (+ a b)))

(defun mod32- (a b)
  "32-bit subtraction"
  (declare (type (unsigned-byte 32) a b))
  (ldb (byte 32 0) (- a b)))

(defun mod32* (a b)
  "32-bit multiplication"
  (declare (type (unsigned-byte 32) a b))
  (ldb (byte 32 0) (* a b)))

(defun mod32ash (num count)
  (declare (type (unsigned-byte 32) num))
  (declare (type integer count))
  (ldb (byte 32 0) (ash num count)))

(defun mod32rol (num count)
  "32-bit rotate left, from H. Warren, Jr.'s Hacker's Delight"
  (declare (type (unsigned-byte 32) num))
  (declare (type integer count))
  (logior (mod32ash num count (mod32ash num (- 0 (- 32 count))))))

(defun mod32ror (num count)
  "32-bit rotate right, from H. Warren, Jr.'s Hacker's Delight"
  (declare (type (unsigned-byte 32) num))
  (declare (type integer count))
  (logior (mod32ash num count (mod32ash num (- 0 (- 32 count))))))
