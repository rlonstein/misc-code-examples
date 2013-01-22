;;;
;;; Compute a SV4R checksum
;;;
;;; compare to gnu coreutils sum output
;;;
;;; R. Lonstein, 2011
;;;
(defun sv4r-sum (file)
  "Calculate a Unix SV4R-style file checksum"
  (with-open-file (stream file :direction :input :element-type 'unsigned-byte)
    (loop :with buffer = (make-array 512 :element-type 'unsigned-byte)
          :and csum = 0
          :for pos = (read-sequence buffer stream)
          :while (plusp pos)
          :do (loop :for b :from 0 :to (1- pos)
                    :do (setf csum (mod (+ csum (svref buffer b)) 65535)))
          :counting buffer :into c
          :finally (return (values csum c)))))
