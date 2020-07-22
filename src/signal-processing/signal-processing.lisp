(in-package #:mfcc)


;;;;;;;;;;;;;;;;;;;
;; DFT Analysis: ;;
;;;;;;;;;;;;;;;;;;;
;;; http://www.bitweenie.com/listings/fft-frequency-axis/
;; The FFT shows the frequency-domain view of a time-domain signal in the frequency space
;; of -fs/2 to fs/2, where fs is the sampling frequency. This frequency space is split
;; into N points, where N is the number of points in the FFT. The spacing between
;; points in frequency domain is simply expressed as:
;;
;; del_R = f_s / N_fft
;;
(defun dft-analysis-core (buffer &key (save-to-file nil)
                                   (audio-filename nil)
                                   (plot-data nil)
                                   (plot-filename nil)
                                   (verbose nil)
                                   (linespace '() linespace-supplied-p))
  "Core function carrying out discrete fourier transform."
  (let* ((padded-array (pad-power-of-two buffer :verbose verbose))
         (raw-complex (napa-fft:rfft padded-array))
         (freq (range 0 :stop (/ *sample-rate* 2)
                        :step (/ *sample-rate*
                                (array-total-size padded-array))))
         (freq-magnitude (make-array  (/ (array-total-size padded-array) 2)
                                     ;;:element-type 'double-float
                                     )))
    (iter
      (for i :index-of-vector freq-magnitude)
      (setf (aref freq-magnitude i) (abs (aref raw-complex i))))

    (when save-to-file
      (save-as-wav-file buffer audio-filename))

    (if plot-data
      (if linespace-supplied-p
          (plot-signal freq plot-filename :y freq-magnitude :linespace linespace)
          (plot-signal freq-magnitude plot-filename)))

    (when verbose
      (format t "~&length of data: ~A" (array-total-size buffer))
      (format t "~&length of padded array: ~A" (array-total-size padded-array))
      (format t "~&length of freq magnitude: ~A" (array-total-size freq-magnitude)))
    freq-magnitude))


;; (dft-analysis-core #(0.2 0.8 9 3 2 3 65 1 3 8 98) :verbose t)

(defun dft-analysis-generated-wave (duration &key (save-to-file nil)
                                           (audio-filename nil audio-filename-supplied)
                                           (plot-fft nil)
                                           (fft-filename nil plot-filename-supplied)
                                           )
  "Takes in wave object and does fft on it."
  (let* ((wave (make-instance 'wave-from-function
                              :duration duration
                              :frequency *frequency*
                              :num-channels *num-channels*))
         (signal-buffer (generate-wave-data wave :with-time t)))
    (dft-analysis-core signal-buffer :save-to-file save-to-file
                                     :audio-filename audio-filename
                                     :plot-data plot-fft
                                     :plot-filename fft-filename)))

#+test
(dft-analysis-generated-wave 2 :save-to-file t
                               :audio-filename "./results/record-gen-fft.wav"
                               :plot-fft t
                               :fft-filename "./results/record-gen-fft.png")

;;; Having an unified core function to handle dft-analysis and multiple API controlling
;;; the source data (e.x: From Mic, Generated Audio etc.,)
(defun dft-analysis-from-mic (duration &key (save-to-file nil)
                                         (audio-filename nil audio-filename-supplied)
                                         (plot-fft nil)
                                         (fft-filename nil plot-filename-supplied))
  "For FFT analysis - Real time audio from Microphone"
  (let* ((audio-data-buffer (record-audio :record-time duration
                                          :dft-analysis t)))
    (dft-analysis-core audio-data-buffer :save-to-file save-to-file
                                         :audio-filename audio-filename
                                         :plot-data plot-fft
                                         :plot-filename fft-filename)))
#+test
(dft-analysis-from-mic 3 :save-to-file t
                         :audio-filename "./results/record-fft.wav"
                         :plot-fft t
                         :fft-filename "./results/record-fft.png")



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pre-emphasis Filter: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre-emphasis filter used on signal mainly to amplify the high frequencies.
;; A pre-emphasis filter is useful in several ways:
;; --- balance the frequency spectrum (By amplifying magnitudes of high fz)
;; --- avoid numerical problems during the Fourier transform operation
;; --- may also improve the Signal-to-Noise Ratio (SNR)
;; The pre-emphasis filter can be applied to a signal x
;; using the first order filter in the following equation:
;;
;; y(t)=x(t)−αx(t−1)
;;
;; pre-emphasized_sample [i] = sample[i] - alpha * sample[i - 1]
;;
;; Filter Coefficient, α (alpha) = 0.95 or 0.97
;; pre_emphasis = 0.97
(defun pre-emphasis-filter (sample-array-buffer &key (alpha 0.97))
  "Takes in sample buffer."
  (let ((sample-len (length sample-array-buffer))
        (preemphasized-samples (copy-seq sample-array-buffer)))
    ;; (format t "~& Copy seq: ~A ~%" preemphasized-samples)
    (iter
     (for idx :from 1 :to (1- sample-len))
     (setf (elt preemphasized-samples idx)
           (- (elt sample-array-buffer idx)
              (* alpha (elt sample-array-buffer (1- idx))))))
    preemphasized-samples))



(defun hann (idx window-len)
    "Hanning window -- ref *Discrete-time Signal Processing - Alan V. Oppenheim, Ronald W. Schafer.*
idx - index of the window
window-len - length of the filter window" 
  (* 0.5 (- 1.0
            (cos (/ (* 2 pi idx)
                        (1- window-len))))))


(defun hamming (idx window-len)
  "Hamming window -- ref *Discrete-time Signal Processing - Alan V. Oppenheim, Ronald W. Schafer.*
idx - index of the window
window-len - length of the filter window" 
  (- 0.54
     (* 0.46
        (cos (/ (* 2 pi idx)
                (1- window-len))))))


(defun triangular-window (begin middle end)
  "Returns the triangular window coordinates for the given input parameters."
  (iter
    (for k :in-sequence (range begin :stop end))
    (collect (cond
               ((or (< k begin) (> k end)) 0.0)
               ((<= k middle) (/ (- k begin)
                                 (- middle begin)))
               ((>= k middle) (/ (- end k)
                                 (- end middle)))) result-type 'vector)))

