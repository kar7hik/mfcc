(in-package #:mfcc)


;;; MFCC
(defun set-frequency-values (mfcc-obj)
  "Setting the maximum and minimum frequency values. Max is half of the sampling rate."
  (setf (max-freq mfcc-obj)
        (floor (/ (sample-rate (audio-obj mfcc-obj))
                  2.0)))
  (setf (min-freq mfcc-obj) 0.0))


(defun frequency-to-mel-scale (freq)
  "Function to convert value from frequency to mel-scale."
  (* 1127 (log (1+ (/ freq 700)) (exp 1))))


(defun mel-scale-to-frequency (mel)
  "Function to convert value from mel-scale to frequency."
  (* 700 (- (exp (/ mel 1127)) 1)))


(defun fbank (min-freq max-freq n-fbank)
  "Returns the Filter bank bins."
  (aops:linspace min-freq (frequency-to-mel-scale max-freq) n-fbank))


(defun mel->frequency-sequence (seq)
  "Returns the list of frequency values corresponding to Mel scale."
  (iter
    (for i in-sequence seq)
    (collect (mel-scale-to-frequency i) result-type 'vector)))


(defun get-frequency-bin (seq &key (nfft *nfft*) (sample-rate *sample-rate*))
  "Returns the bin containing frequencies."
  (iter
    (for elt in-sequence seq)
    (collect (floor (/ (* (1+ nfft)
                          elt)
                       sample-rate)) result-type 'vector)))


(defun get-filter-banks (bin &key (nfft *nfft*))
  "Returns the list of filter bank."
  (iter
    (for i :from 1 :below (- (length bin) 1))
    (let* ((begin (elt bin (- i 1)))
           (middle (elt bin i))
           (end (elt bin (1+ i)))
           (fbank (triangular-window begin middle end))
           (start (floor begin))
           (out (aops:zeros* 'single-float (/ nfft 2))))
      (iter
        (for i :from start :below (length out))
        (for j :in-sequence fbank)
        (setf (aref out i) j))
      (collect out))))



(defun get-log-mel-features (filter-banks)
  "Applies log to the filter bank values."
  (with-gensyms (log-banks)
    (let ((row (get-row filter-banks))
          (col (get-col filter-banks)))
      (setf log-banks (make-array `(,row ,col) :element-type 'double-float
                                               :initial-element 0d0))
      (iter
        (for i :from 0 :below row)
        (iter
          (for j :from 0 :below col)
          (when (zerop (aref filter-banks i j))
            (setf (aref filter-banks i j) (coerce most-negative-double-float 'double-float)))
          (setf (aref log-banks i j)
                (log (aref filter-banks i j)))))
      log-banks)))



(defun get-dct-mel-features (dct-mel-values num-cepstrum)
  (let ((row (get-row dct-mel-values))
        (reduced-dct (compute (fuse dct-mel-values))))
    (iter
      (for i :from 0 :below row)
      (collect (aops:subvec (aops:sub reduced-dct i) 0  num-cepstrum) result-type 'vector))))



(defun set-frequency-bins (mfcc-obj)
  (setf (bins mfcc-obj)
        (coerce-sequence
         (get-frequency-bin (mel->frequency-sequence (fbank (min-freq mfcc-obj)
                                                            (max-freq mfcc-obj)
                                                            (+ 2 (num-filters mfcc-obj)))))
         'single-float)))


(defun set-filter-banks (mfcc-obj)
  (setf (filter-banks mfcc-obj)
        (list-to-2d-array (get-filter-banks (bins mfcc-obj)))))


(defun set-log-mel-features (mfcc-obj)
  (let* ((raw-mult (matrix-multiplication (power-spectrum (audio-obj mfcc-obj))
                                          (compute (transpose (filter-banks mfcc-obj)))))
         (log-mel (get-log-mel-features raw-mult)))
    (setf (raw-mel-features mfcc-obj) raw-mult
          (log-mel-features mfcc-obj) log-mel)))


(defun set-dct-mel-features (mfcc-obj)
  (let* ((dct-mel (apply-dct (log-mel-features mfcc-obj)))
         (reduced-mel (aops:combine (get-dct-mel-features dct-mel (num-cepstrum mfcc-obj)))))
    (setf (dct-mel-values mfcc-obj) dct-mel
          (reduced-mel-values mfcc-obj) reduced-mel)))



(defun apply-dct (log-filter-banks)
  "Applies DCT-II on the filter banks."
  (let ((row (get-row log-filter-banks)))
    (iter
      (for i :from 0 :below row)
      (collect (dct:dct (aops:sub log-filter-banks i)) result-type 'vector))))



;;; Liftering:
(defun set-lifter (mfcc-obj)
  "Returns the vector containing the liftering values as the size of num-cepstrum."
  (destructuring-bind (num-frames num-cepstrum)
      (array-dimensions (reduced-mel-values mfcc-obj))
    (declare (ignore num-frames))
    (let ((lift (range 0 :stop (1- num-cepstrum))))
      (iter
        (for i :in-vector lift)
        (collect (1+ (* (/ (cepstrum-lifter mfcc-obj) 2)
                        (sin (* pi (/ i (cepstrum-lifter mfcc-obj))))))
          result-type 'vector)))))


(defun apply-liftering-to-dct (mfcc-obj)
  "The liftering is applied on the reduced MEL values."
  (let ((lift (set-lifter mfcc-obj)))
    (compute (alpha #'*
                    lift
                    (reduced-mel-values mfcc-obj)))))


(defun get-mean-normalization (2d-array)
  "Returns the mean values of the 2d-array with axis 0."
  (let ((arr-mean (2d-mean 2d-array)))
    (compute (alpha #'- 2d-array arr-mean))))



;; (defparameter *mel* (reduced-mel-values *mfcc-obj*))

;; (defparameter *del* (iter
;;                       (for i :from 1 :below (get-row *mel*))
;;                       (collect
;;                           (compute (alpha #'- (aops:sub *mel* i)
;;                                           (aops:sub *mel* (- i 1)))))))


(defun pad-array (array &key (row-above 0)
                          (row-below 0)
                          (col-right 0)
                          (col-left 0)
                          (pad-with 'edge))
  (declare (ignore pad-with))
  (alexandria:with-gensyms (result-array)
    (let ((rows (+ row-above
                    row-below
                    (get-row array)))
          (cols (+ col-right
                   col-left
                   (get-col array)))
          (element-type (array-element-type array)))
      (setf result-array
            (make-array `(,rows ,cols)
                        :element-type element-type
                        :initial-element (coerce 0 element-type)))
      (dotimes (i (get-row array))
        (dotimes (j (get-col array))
          (setf (aref result-array (+ i row-above) (+ j col-left))
                (aref array i j))))
      (when row-above
        (dotimes (i row-above)
          (dotimes (j (get-col array))
            (setf (aref result-array i (+ j col-left))
                  (aref array 0 j)))))
      (when row-below
        (dotimes (i row-below)
          (dotimes (j (get-col array))
            (setf (aref result-array
                        (+ i (get-row array) row-above)
                        (+ j col-left))
                  (aref array (1- (get-row array)) j)))))
      (when col-left
        (dotimes (i rows)
          (dotimes (j col-left)
            (setf (aref result-array
                        i j)
                  (aref result-array i col-left)))))
      (when col-right
        (dotimes (i rows)
          (dotimes (j col-right)
            (setf (aref result-array
                        i (+ j col-left (get-col array)))
                  (aref result-array i (+ (1- (get-col array)) col-left))))))
      result-array)))



;; (defparameter *test* #2A((1 2 3)
;;                          (4 5 6)
;;                          (7 8 9)
;;                          (3 5 6)
;;                          (8 5 2)
;;                          (1 3 3)
;;                          (5 8 6)
;;                          (7 7 7)
;;                          (3 1 4)
;;                          (1 0 9)))

;; (defparameter *padd* (pad-array *test* :row-above 2 :row-below 2))



(defun create-empty-array-like (array &key (type 'single-float) (initial-value 0.0))
  "Returns a empty array with same dimension as the input array. Default initial values are 0.0. Default return type is single-float."
  (make-array (array-dimensions array) :element-type type
                                       :initial-element (coerce initial-value type)))


(defun delta (feature-array N)
  (declare (type (simple-array * *) feature-array))
  (let* ((padded-array (pad-array feature-array :row-above N :row-below N))
         (multiplier (range (* -1 N) :stop N))
         (stride-size (length multiplier))
         (col (get-col feature-array))
         (denominator 1.0))
    (declare (type (simple-array * *) padded-array)
             (type (simple-array * *) multiplier)
             (type fixnum stride-size col))
    (setf denominator
          (* N (compute (beta #'+
                              (iter
                                (for i :from 1 :below (1+ N))
                                (collect (expt i 2) result-type 'vector))))))
    (compute (alpha #'/
                    (aops:combine
                     (iter
                       (for row :from 0 :below (get-row feature-array))
                       (collect (compute
                                 (beta #'+
                                       (transpose
                                        (alpha #'*
                                               multiplier
                                               (transpose
                                                (reshape
                                                 padded-array (~ row (+ row (1- stride-size))
                                                                 ~ 0 (1- col))))))))
                         result-type 'vector)))
                    denominator))))






;; (defun delta-1 (feature-array N)
;;   (declare (type (simple-array * *) feature-array))
;;   (let* ((padded-array (pad-array feature-array :row-above N :row-below N))
;;          (multiplier (range (- N) :stop N))
;;          (stride-size (length multiplier))
;;          (col (get-col feature-array)))
;;     (compute (alpha #'/
;;                     (aops:combine
;;                      (iter
;;                        (for row :from 0 :below (get-row feature-array))
;;                        (collect (compute (beta #'+
;;                                                (aops:combine
;;                                                 (let ((temp (compute (reshape padded-array (~ row (+ row (1- stride-size)) ~ 0 (1- col))))))
;;                                                   (iter
;;                                                     (for i :from row :below (+ row stride-size))
;;                                                     (for j :from 0 :below stride-size)
;;                                                     (collect
;;                                                         (compute
;;                                                          (alpha #'*
;;                                                                 (aref multiplier j)
;;                                                                 (aops:sub temp j)))
;;                                                       result-type 'vector))))))
;;                          result-type 'vector)))
;;                     10.0))))


;; (time (defparameter *fin* (delta-1 *lift* 2)))
(declaim (ftype (function ((simple-array float *)) (simple-array * *)) find-energy))
(defun find-energy (power-spectrum-array)
  (alexandria:with-gensyms (energy-result)
    (let ((array-size (get-row power-spectrum-array)))
      (declare (type fixnum array-size))
      (setf energy-result
            (make-array array-size
                        :element-type 'double-float
                        :initial-element 0d0))
      (dotimes (i array-size)
        (setf (aref energy-result i)
              (the double-float (reduce #'+ (aops:sub power-spectrum-array i))))))
    energy-result))





;; (defun energy (power-spectrum-array)
;;   (alexandria:with-gensyms (energy-result)
;;     (let ((array-size (array-total-size power-spectrum-array)))
;;       (setf energy-result
;;             (make-array array-size
;;                         :element-type 'double-float
;;                         :initial-element 0d0))
;;       )
;;     energy-result))



