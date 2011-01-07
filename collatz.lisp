;;;
;;; http://streamtech.nl/problemset/100.html
;;;

(defun collatz (int &optional show )
  "calculate the collatz sequence for a positive integer"
  ;; http://en.wikipedia.org/wiki/Collatz_conjecture
  (declare (type fixnum int))
  (loop :with n = int
        :when (not (null show)) :do (format t "~D -> ~D~%" c n)
        :while (> n 1)
        :counting n :into c
        :do (if (oddp n) (setf n (1+ (* 3 n)))
                (setf n (/ n 2)))
        :finally (return (1+ c))))


(defun max-cycle-in-range (i j)
  "return the length of the longest Collatz sequence in the given integer range"
  (declare (type integer i j))
  (when (> i j) (rotatef i j))
  (loop :for n :from i :to j
        :maximize (collatz n)))


(defun read-only-number (&optional (input-stream t) (eof-error-p t) eof-value recursive)
  (loop :for item = (read input-stream eof-error-p eof-value recursive)
        :until (numberp item)
        :finally (return item)))


(defun problem100 (file)
  (with-open-file (s file)
    (loop :with line
          :while (setf line (read-line s nil nil))
          :for in = (make-string-input-stream line)
          :do (let ((start (read-only-number in))
                    (end (read-only-number in)))
                (format t "~D ~D ~D~%" start end (max-cycle-in-range start end))))))
