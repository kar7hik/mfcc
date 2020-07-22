(in-package #:mfcc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Discrete Fourier Transform ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Discrete Time Fourier Transform (DTFT) computes an infinite number of frequency bins,
;;; and the Discrete Fourier Transform (DFT) picks out a finite number of those.

;;; Zero padding for FFT:

;;; https://www.mechanicalvibration.com/Zero_Padding_FFTs.html
;; "Zero-padding" means adding additional zeros to a sample of data
;; FFT is slow for prime numbers, but much faster for powers of two. We can add an extra
;; zero to the end of the sample and thus get much better performance.

;; The other reason that zero-padding is used is to get better frequency resolution
;; Zero padding allows us to take more samples of the DTFT.
;; For example, if we have 1000 points of data, sampled at 1000 Hz, and perform the standard
;; FFT, we get a frequency bin every 1 Hz. But if we pad with 1000 zeros and then run a 2000
;; point FFT, now we get frequency bins every 0.5 Hz. This allows us to get around some of
;; the disadvantages of the DFT
(defun pad-power-of-two (sample-list &key (verbose nil))
  "Function takes in sample list, checks for power of 2.
If sample length is not power of 2, pads zero at the end of the sample list."
  (let* ((sample-length (array-total-size sample-list))
         (power (ceiling (log sample-length 2)))
         (required-sample-length (expt 2 power))
         (number-of-samples-added (- required-sample-length
                                     sample-length))
         (offset (floor (/ number-of-samples-added 2)))
         (array (make-array (list required-sample-length)
;                            :element-type 'double-float
                            )))
    (setf (subseq array offset (+ sample-length offset))
          sample-list)
    ;; For verbose output:
    (when verbose
      (format t "~&sample length: ~A~&power: ~A" sample-length power)
      (format t "~&required: ~A~&number of sample: ~A ~%" required-sample-length
              number-of-samples-added))
    array))





