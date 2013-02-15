;;;
;;; Authenticator for Google based on RFC-4226 (HOTP) and RFC-6238
;;; (TOTP) in Chicken Scheme
;;;
;;; Copyright (c) 2013, Ross Lonstein
;;;
;;; Released under the BSD license, http://opensource.org/licenses/BSD-2-Clause
;;;

(use srfi-1 srfi-4 blob-set-int hmac sha1)

(declare (uses base32))

(define (trunc str len)
  (define (extract-int32 str)                                           ; annoying string->blob->vector
    (let* ((u8vec (blob->u8vector (string->blob str)))                  ; conversions because hmac digest
           (offset (bitwise-and                                         ; outputs strings.
                    (u8vector-ref u8vec (sub1 (u8vector-length u8vec))) ; TODO: clean up/refactor everywhere
                    #x0f)))
      ;; bit fiddling right out of RFC-4226, see Sec 5.4...
      (bitwise-ior
       (arithmetic-shift (bitwise-and (u8vector-ref u8vec offset) #x7f) 24)
       (arithmetic-shift (bitwise-and (u8vector-ref u8vec (+ 1 offset)) #xff) 16)
       (arithmetic-shift (bitwise-and (u8vector-ref u8vec (+ 2 offset)) #xff) 8)
       (bitwise-and (u8vector-ref u8vec (+ 3 offset)) #xff))))
  ;;
  (number->string (modulo (extract-int32 str) (expt 10 len))))


(define (hotp k c len)
  (let ((hash ((hmac k (sha1-primitive)) c))) ; N.B. "hash" is string, see TODO
    (trunc hash len)))


(define (totp key interval len)
  (let ((c  (inexact->exact (truncate (/ (current-seconds) interval))))
        (msg (make-blob 8)))
    (blob-set-u64-be! msg c)
    (hotp key (blob->string msg) len))) ; blob->string ugly, maybe see TODO


(define (gauth secret)
  (let ((k (base32-decode secret)))
    (totp k 30 6)))


;; top-level invocation
(print (gauth (second (argv))))
