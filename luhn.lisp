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

(defun luhn-check (num)
  (let ((luhn-double #(0 2 4 6 8 1 3 5 7 9))
        (d (reverse (digits num))))
    (loop :with checksum = 0
          :for n :in d
          :for alternate = t :then (not alternate)
          :do (setf checksum (+ checksum (if alternate (svref luhn-double n) n)))
          :finally (return (mod (- 10 (mod checksum 10)) 10)))))

(defun luhn-p (num)
  (the integer num)
  (let* ((luhn-double #(0 2 4 6 8 1 3 5 7 9))
         (d (reverse (digits num)))
         (checksum (pop d)))
    (loop :for n :in d
          :for alternate = t :then (not alternate)
          :do (setf checksum (+ checksum (if alternate (svref luhn-double n) n)))
          :finally (return (mod checksum 10)))))
