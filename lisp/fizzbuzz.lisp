;;;
;;; fizzbuzz.lisp
;;;
;;; common-lisp implementation of the the classic programming
;;; interview question
;;;
;;; R. Lonstein, 2011
;;;

(defun fizzbuzz (n)
  "print a series from 1 to supplied integer, where evenly
divisible by 3 output 'fizz', where evenly divisible by 5 output 'buzz',
where evenly divisible by both output 'fizzbuzz'"
  (declare (type (integer 1) n))
  (loop :for i :from 1 :to n
        :for div3 = (zerop (rem i 3))
        :for div5 = (zerop (rem i 5))
        :for out = (cond ((and div3 div5) "fizzbuzz")
                         (div3 "fizz")
                         (div5 "buzz")
                         (t i))
        :do (format t "~A " out)))
