;;;
;;; Verhoeff decimal error detection
;;;
;;; http://en.wikipedia.org/wiki/Verhoeff_algorithm
;;;

(defvar verhoeff-d5-table
  #2A((0 1 2 3 4 5 6 7 8 9)
      (1 2 3 4 0 6 7 8 9 5)
      (2 3 4 0 1 7 8 9 5 6)
      (3 4 0 1 2 8 9 5 6 7)
      (4 0 1 2 3 9 5 6 7 8)
      (5 9 8 7 6 0 4 3 2 1)
      (6 5 9 8 7 1 0 4 3 2)
      (7 6 5 9 8 2 1 0 4 3)
      (8 7 6 5 9 3 2 1 0 4)
      (9 8 7 6 5 4 3 2 1 0)))

(defvar verhoeff-p-table
  #2A((0 1 2 3 4 5 6 7 8 9)
      (1 5 7 6 2 8 3 0 9 4)
      (5 8 0 3 7 9 6 1 4 2)
      (8 9 1 6 0 4 3 5 2 7)
      (9 4 5 3 1 2 6 8 7 0)
      (4 2 8 6 5 7 3 9 0 1)
      (2 7 9 3 8 0 6 4 1 5)
      (7 0 4 6 9 1 3 2 5 8)))

(defvar verhoeff-inv-table #(0 4 3 2 1 5 6 7 8 9))

(defun digits (n)
  "Convert an integer to a list of digits"
  (the integer n)
  (loop :with str = (princ-to-string n)
        :for chr :across str
        :for digit = (digit-char-p chr)
        :when digit
        :collect digit))


(defun verhoeff-check (num)
  "Validate a Verhoeff checksum"
  ;; TODO: ?? if zeroth digit is zero, use inv() as check digit ??
  (loop :with checksum = 0
        :for digit :in (reverse (digits num))
        :for permutation = (aref verhoeff-p-table (mod i 8) digit)
        :for dihedral = (aref verhoeff-d5-table checksum permutation)
        :counting digit :into i
        :do (setf checksum dihedral)
        :finally (return (zerop checksum))))
