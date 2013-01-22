;;;
;;; Common Lisp implementation of the Unix V7 fortune
;;;

(defun fortune (filename)
  "Return a Unix V7 fortune"
  (with-open-file (s filename :direction :input)
    (loop :with fortune
          :for line = (read-line s nil)
          :while line
          :counting line :into counter
          :do (when (< (random counter) 1) (setq fortune line))
          :finally (return fortune))))
