(in-package #:mfcc)

(defun find-frame-length (sample-rate frame-size)
  "Function returns the frame length. Convert from seconds to samples"
  (round (* frame-size sample-rate)))


(defun find-frame-step (sample-rate frame-stride)
  "Function returns the frame step. Convert from seconds to samples."
  (round (* frame-stride sample-rate)))


(defun set-audio-length (signal-processing-obj)
  "Function to set the audio-length of the signal-processing instances."
  (setf (audio-data-length signal-processing-obj)
        (find-audio-length (audio-data signal-processing-obj)
                           (num-channels signal-processing-obj))))
  
(defun set-frame-length (signal-processing-obj)
  "Function to set the frame length of the signal processing instances."
  (setf (frame-length signal-processing-obj)
        (find-frame-length (sample-rate signal-processing-obj)
                           (frame-size signal-processing-obj))))

(defun set-frame-step (signal-processing-obj)
  "Function to set the frame step of the signal processing instances."
  (setf (frame-step signal-processing-obj)
        (find-frame-step (sample-rate signal-processing-obj)
                         (frame-stride signal-processing-obj))))

(defun set-total-number-of-frames (signal-processing-obj)
  (setf (total-number-of-frames signal-processing-obj)
        (ceiling (float (/ (abs (- (audio-data-length signal-processing-obj)
                                   (frame-length signal-processing-obj)))
                           (frame-step signal-processing-obj))))))


(defun set-pad-audio-length (signal-processing-obj)
  (setf (pad-audio-length signal-processing-obj)
        (+ (* (total-number-of-frames signal-processing-obj)
              (frame-step signal-processing-obj))
           (frame-length signal-processing-obj))))


(defun set-padding-required (signal-processing-obj)
  (setf (padding-required signal-processing-obj)
        (- (pad-audio-length signal-processing-obj)
           (audio-data-length signal-processing-obj))))


(defun set-splitted-frames (signal-processing-obj)
  (setf (splitted-frames signal-processing-obj)
        (aops:combine (coerce (split-audio-into-frames (padded-frames signal-processing-obj)
                                                       (frame-length signal-processing-obj)
                                                       (frame-step signal-processing-obj)
                                                       :padded-p t)
                              'vector))))
  

(defun set-filtered-frames (signal-processing-obj)
  (setf (filtered-frames signal-processing-obj)
        (aops:combine (coerce (apply-filter-to-audio (splitted-frames signal-processing-obj)) 'vector))))




(defun flatten-list (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))


;;; Applying pre-emphasis on audio-data. This function deals with stereo audio.
(defun apply-preemphasis-sequence (sequence &key (alpha 0.97))
  (iter
  (for i :from 1 :below (length sequence))
    (collect (mapcar #'-
                     (aref sequence i)
                     (mapcar (lambda (x)
                               (* x alpha))
                             (aref sequence (- i 1)))))))


;;; Splitting audio data into frames:
(defun split-padded-audio-into-frames (audio-data frame-length frame-step &key (verbose nil))
  (let* ((audio-data-length (length audio-data))
         (total-num-frames (floor (/ audio-data-length frame-step)))
         (result-list-frames '()))
    (iter
      (for i :from 0 :below (1- total-num-frames))
      (let* ((start-idx (* i frame-step))
             (end-idx (+ frame-length start-idx))
             (splitted-frame '()))
        (when verbose
          (format t "~&~% start-idx: ~A ~%" start-idx)
          (format t "~& end-idx: ~A ~%" end-idx)
          (format t "~& total-num-frames: ~A and i: ~A  ~%" total-num-frames i)
          (format t "~& audio-data-length: ~A ~%" audio-data-length))
        (when (< audio-data-length end-idx)
          (setf splitted-frame (append (subseq audio-data start-idx total-num-frames)
                                       (zeros (- end-idx total-num-frames)))))
        (setf splitted-frame (subseq audio-data start-idx end-idx))
        (push splitted-frame result-list-frames)))
    (nreverse result-list-frames)))



;; TODO: Replace the following in split-padded-audio-into-frames function
;; (append (subseq '(0 1 2 3 4) 2 4) (subseq '(0 1 2 3 4) 2 4))
;; (concatenate 'vector (aops:subvec #(0 1 2 3 4) 2 4) (aops:subvec #(0 1 2 3 4) 2 4))


(defun split-audio-into-frames (audio-data frame-length frame-step &key (padded-p t))
  (if padded-p
      (split-padded-audio-into-frames audio-data frame-length frame-step)
      (progn
        (format t "~& The audio data is padded with additional frames for computation. ~%")
        (setf audio-data (pad-audio-with-proper-zeros audio-data))
        (split-padded-audio-into-frames audio-data frame-length frame-step))))


;;; Applying Filter to the audio:
(defun apply-filter-to-frame (frame &key (filter-function 'hamming))
  "Function that applies the selected type of window to the frame."
  (let* ((frame-len (length frame))
         (filtered-frame (make-array frame-len
                                     :element-type 'double-float)))
    (iter
      (for i :from 0 :below frame-len)
      (setf (aref filtered-frame i)
            (* (funcall filter-function i frame-len)
               (aref frame i))))
    filtered-frame))


(defun apply-filter-to-audio (splitted-frames &key (filter-function 'hamming))
  (iter
    (for i :from 0 :below (get-row splitted-frames))
    (collect (apply-filter-to-frame (aops:sub splitted-frames i)
                                    :filter-function filter-function))))


(defun pad-frame (frame pad-length)
  (declare (type (simple-array * *) frame))
  (alexandria:with-gensyms (pad-result)
    (setf pad-result
          (make-array pad-length))
    (dotimes (i (length frame))
      (setf (aref pad-result i)
            (aref frame i)))
    pad-result))


(defun pad-frame* (frame pad-length &key (type 'single-float))
  ;; (declare (type (simple-array * *) frame))
  (alexandria:with-gensyms (pad-result)
    (setf pad-result
          (make-array pad-length
                      :element-type 'single-float
                      :initial-element (coerce 0 type)))
    (dotimes (i (get-row frame))
      (setf (aref pad-result i)
            (coerce (aref frame i) type)))
    pad-result))


;;; Applying FFT:
(defun apply-fft-to-frame (frame &key (nfft nil nfft-supplied-p))
  "Given a frame, this function applies FFT to it."
  (alexandria:with-gensyms (fft-result)
    (if nfft-supplied-p
        (setf fft-result
              (dft-analysis-core (pad-frame* frame nfft)))
        (setf fft-result
              (dft-analysis-core (pad-power-of-two frame))))
    fft-result))


(defun apply-fft-to-audio (filtered-audio-data  &key (nfft *nfft*))
  "Given a audio-data, the function returns the FFT of the it."
  (let ((total-num-frames (get-row filtered-audio-data)))
    (iter
      (for i :from 0 :below total-num-frames)
      (collect (apply-fft-to-frame (aops:sub filtered-audio-data i) :nfft nfft)))))



;;; Finding Power Spectrum:
(defun apply-power-spectrum (frame NFFT)
  "Computes the power spectrum of frames."
  (iter
    (for elt :in-sequence frame)
    (collect (/ (expt elt 2) NFFT) result-type 'vector)))


(defun find-power-spectrum-audio (frames NFFT)
  "Given the frames, the function returns the Power spectrum."
  (iter
    (for frame :in-sequence frames)
    (collect (apply-power-spectrum frame NFFT) result-type 'vector)))


