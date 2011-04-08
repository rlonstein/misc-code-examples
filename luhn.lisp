;;;
;;; Luhn 10 Checksum
;;;

(defun digits (n)
  (the integer n)
  (loop :with str = (princ-to-string n)
        :for chr :across str
        :for digit = (digit-char-p chr)
        :when digit
        :collect digit))

(defun luhn-helper (num &key (verify t))
  "Calculate or Verify a Luhn 10 checksum"
  ;; refactored nearly identical routines that encode/decode the luhn
  ;; checksum into one that does both
  (let* ((luhn-double #(0 2 4 6 8 1 3 5 7 9))
         (d (reverse (digits num)))
         (checksum (if verify (pop d) 0)))
    (loop :for n :in d
          :for alternate = t :then (not alternate)
          :do (setf checksum (+ checksum (if alternate (svref luhn-double n) n)))
          :finally (if verify (return (mod checksum 10))
                       (return (mod (- 10 (mod checksum 10)) 10))))))

(defun luhn-check (num)
  "Calculate a Luhn 10 checksum for a number"
  (luhn-helper num :verify nil))

(defun luhn-p (num)
  "Verify a Luhn 10 checksummed number"
  (luhn-helper num))
