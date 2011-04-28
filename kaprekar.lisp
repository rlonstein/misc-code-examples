;;;
;;; Calculate the cycles to reach a Kaprekar constant
;;;
;;; given a four digit number where not all digits are identical, sort
;;; the digits in ascending and descending order then subtract the
;;; greater from the lesser to produce a result.  A "chain" is then to
;;; repeat with the result until a constant of 6174 (4-digit) or 495
;;; (3-digit) is reached
;;;

(defun kaprekar (n)
  (declare (type fixnum n))
  (let* ((s (sort (prin1-to-string n) #'char<))
         (n1 (parse-integer s))
         (n2 (parse-integer (reverse s))))
    (declare (type fixnum n1 n2))
    (when (> n2 n1) (rotatef n2 n1))
    (values (- n1 n2) n1 n2)))

(defun kaprekar-chain (n)
  (loop
    :for (r n1 n2) = (multiple-value-list (kaprekar n)) :then (multiple-value-list (kaprekar r))
    :counting r :into c
    :do (format t "~S - ~S-~S=~S~%" c n1 n2 r)
    :until (or (= r n) (= r 6174) (= r 0))))
