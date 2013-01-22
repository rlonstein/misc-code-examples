;;;
;;; primewords.lisp
;;;
;;; Treat a word as a base36 number (0-9A-Z) and
;;; determine if that number is prime or composite
;;;
;;; R. Lonstein, 2011
;;;

(defun prime-p (n)
  ;; TODO: verify this algorithm
  (declare (type (integer 0) n))
  (if (or (zerop n) (= 1 n)) nil
      (loop :with midpoint = (truncate (sqrt n))
            :for f :from 2 :to midpoint
            :when (zerop (rem n f)) return (values nil f)
            :finally (return t))))


(defun word->b36value (str)
  (declare (type (simple-string) str))
  (loop
    :with b36set = #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                     #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
                     #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
                     #\U #\V #\W #\X #\Y #\Z)
    :for c :across (reverse (string-upcase str))
    :counting c :into offset
    :summing (* (expt 10 (1- offset)) (position c b36set))))


(defun primewords (str)
  (if (prime-p (word->b36value str)) (format t "prime")
      (format t "composite")))
