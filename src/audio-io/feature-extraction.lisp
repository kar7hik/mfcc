(in-package #:mfcc)

(defparameter *frame-size* 0.020
  "Short frame size in ms.")

(defparameter *frame-stride* 0.01
  "Frame stride size in ms.")

(defparameter *NFFT* 1024
  "FFT bin value.")

(defparameter *num-filters* 40
  "Number of Filter bank.")

(defparameter *num-cepstrum* 13
  "Number of Cepstrum.")

(defparameter *cepstrum-lifter* 22)


(defclass signal-processing (wave-signal)
  ((audio-data :initarg :audio-data
               :accessor audio-data
               :type (simple-array *))
   (frame-size :initarg :frame-size
               :initform *frame-size*
               :accessor frame-size
               :type 'single-float)
   (frame-stride :initarg :frame-stride
                 :initform *frame-stride*
                 :accessor frame-stride
                 :type 'single-float)
   (frame-length :initarg :frame-length
                 :accessor frame-length
                 :type 'fixnum)
   (sample-rate :initarg :sample-rate
                :initform *sample-rate*
                :accessor sample-rate
                :type 'double-float)
   (total-number-of-frames :initarg :total-number-of-frames
                           :accessor total-number-of-frames
                           :type 'fixnum)
   (pre-emphasis :initarg :pre-emphasis
                 :initform 0.97
                 :accessor pre-emphasis
                 :type 'single-float)
   (emphasized-signal :initarg :emphasized-signal
                      :accessor emphasized-signal
                      :type (simple-array *))
   (frame-step :initarg :frame-step
               :accessor frame-step
               :type 'fixnum)
   (audio-data-length :initarg :audio-data-length
                      :accessor audio-data-length
                      :type 'fixnum)
   (pad-audio-length :initarg :pad-audio-length
                     :accessor pad-audio-length
                     :type 'integer)
   (padding-required :initarg :padding-required
                     :accessor padding-required
                     :type 'integer)
   (padded-frames :initarg :padded-frames
                  :accessor padded-frames
                  :type (simple-array *))
   (filtered-frames :initarg :padded-frames
                    :accessor filtered-frames
                    :type (simple-array double-float *))
   (splitted-frames :initarg :splitted-frames
                    :accessor splitted-frames
                    :type (simple-array double-float *))
   (fft-applied-p :initarg :fft-applied-p
                    :initform nil
                    :type 'boolean
                    :accessor fft-applied-p
                    :documentation "Flag to check whether the FFT has been applied already.")
   (NFFT :initarg :NFFT
         :initform *nfft*
         :accessor NFFT
         :type 'integer
         :documentation "Fourier Transform bin length.")
   (fft-result :initarg :fft-result
               :type (simple-array single-float)
               :accessor fft-result
               :documentation "List of simple arrays containing the result of FFT.")
   (power-spectrum :initarg :power-spectrum
                   :accessor power-spectrum
                   :documentation "List of simple arrays - power spectrum values of fft-results."
                   :type (simple-array double-float *))
   (energy :initarg :energy
           :accessor energy
           :documentation "Vector containing energy values. Sum of rows of power spectrum."
           :type (simple-array *)))
  
  (:documentation "Signal processing Class"))

(defun make-signal-processing (audio-data num-channels)
  "Constructor function for Signal processing class."
  (alexandria:with-gensyms (signal-processing-obj)
    (setf signal-processing-obj
          (make-instance 'signal-processing
                         :audio-data audio-data
                         :num-channels num-channels))
    signal-processing-obj))


(defclass mfcc ()
  ((audio-obj :initarg :audio-obj
              :accessor audio-obj)
   (num-filters :initarg :num-filters
                :initform *num-filters*
                :accessor num-filters
                :type 'fixnum)
   (bins :initarg :bins
         :accessor bins
         :type (simple-array *))
   (num-cepstrum :initarg :num-cepstrum
                 :initform *num-cepstrum*
                 :accessor num-cepstrum
                 :type 'fixnum)
   (min-freq :initarg :min-freq
             :initform 0.0
             :accessor min-freq
             :type 'single-float)
   (max-freq :initarg :max-freq
             :initform 0.0
             :accessor max-freq
             :type 'single-float)
   (cepstrum-lifter :initarg :cepstrum-lifter
                    :initform *cepstrum-lifter*
                    :accessor cepstrum-lifter
                    :type 'fixnum)
   (filter-banks :initarg :filter-banks
                 :accessor filter-banks
                 :type (simple-array double-float *))
   (raw-mel-features :initarg :raw-mel-features
                     :accessor raw-mel-features
                     :type (simple-array double-float *))
   (log-mel-features :initarg :log-mel-features
                     :accessor log-mel-features
                     :type (simple-array double-float *))
   (dct-mel-values :initarg :dct-mel-values
                   :accessor dct-mel-values
                   :type (simple-array double-float *))
   (reduced-mel-values :initarg :reduced-mel-values
                       :accessor reduced-mel-values
                       :type (simple-array single-float *)))  
  (:documentation "MFCC Class"))


(defun make-mfcc (audio-obj &key (num-filters *num-filters*)
                              (cepstrum-lifter *cepstrum-lifter*)
                              (num-cepstrum *num-cepstrum*))
  "Constructor function for MFCC Class."
  (alexandria:with-gensyms (mfcc-obj)
    (setf mfcc-obj (make-instance 'mfcc :audio-obj audio-obj
                                        :num-filters num-filters
                                        :cepstrum-lifter cepstrum-lifter
                                        :num-cepstrum num-cepstrum))
    mfcc-obj))



(defgeneric pre-emphasis-audio-data (signal-processing-obj)
  (:documentation "Generic function to apply pre-emphasis on the audio data."))


(defmethod pre-emphasis-audio-data ((signal-processing-obj signal-processing))
  (when (= (num-channels signal-processing-obj) 1)
    (setf (emphasized-signal signal-processing-obj)
          (pre-emphasis-filter (audio-data signal-processing-obj))))
  (when (> (num-channels signal-processing-obj) 1)
    (setf (emphasized-signal signal-processing-obj)
          (coerce
           (flatten-list
            (concatenate 'list (aref (audio-data signal-processing-obj) 0)
                         (apply-preemphasis-sequence (audio-data signal-processing-obj))))
           'vector))))


(defgeneric pad-audio-with-zeros (signal-processing-obj)
  (:documentation "Finds the required padding for the audio signal and pads the zeros."))


(defmethod pad-audio-with-zeros ((signal-processing-obj signal-processing))
  (setf (padded-frames signal-processing-obj)
        (concatenate 'vector (emphasized-signal signal-processing-obj)
                     (zeros (padding-required signal-processing-obj) :type 'double-float))))


(defmethod set-fft-result ((signal-processing-obj signal-processing))
  (setf (fft-result signal-processing-obj)
        (apply-fft-to-audio (filtered-frames signal-processing-obj)
                            :nfft (nfft signal-processing-obj))))


(defmethod set-power-spectrum ((signal-processing-obj signal-processing))
  (setf (power-spectrum signal-processing-obj)
        (aops:combine (find-power-spectrum-audio (fft-result signal-processing-obj)
                                                 (nfft signal-processing-obj)))))


(defmethod set-energy ((signal-processing-obj signal-processing))
  (setf (energy signal-processing-obj)
        (find-energy (power-spectrum signal-processing-obj))))


(defmethod initialize-instance :after ((signal-processing-obj signal-processing) &key)
  (set-audio-length signal-processing-obj)
  (set-frame-length signal-processing-obj)
  (set-frame-step signal-processing-obj)
  (pre-emphasis-audio-data signal-processing-obj)
  (set-total-number-of-frames signal-processing-obj)
  (set-pad-audio-length signal-processing-obj)
  (set-padding-required signal-processing-obj)
  (pad-audio-with-zeros signal-processing-obj)
  (set-splitted-frames signal-processing-obj)
  (set-filtered-frames signal-processing-obj)
  (set-fft-result signal-processing-obj)
  (set-power-spectrum signal-processing-obj)
  (set-energy signal-processing-obj))


(defun find-frequency-bins (mfcc-obj)
  (setf (bins mfcc-obj)
        (coerce-sequence
         (get-frequency-bin (mel->frequency-sequence (fbank (min-freq mfcc-obj)
                                                            (max-freq mfcc-obj)
                                                            (+ 2 (num-filters mfcc-obj)))))
                         'single-float)))

(defmethod initialize-instance :after ((mfcc-obj mfcc) &key)
  (set-frequency-values mfcc-obj)
  (set-frequency-bins mfcc-obj)
  (set-filter-banks mfcc-obj)
  (set-log-mel-features mfcc-obj)
  (set-dct-mel-features mfcc-obj))


(defparameter *wav-file* "/home/karthik/quicklisp/local-projects/signal/music-mono-1.wav")
;; (defparameter *wav-file* "/home/karthik/quicklisp/local-projects/signal/music-mono.wav")
;; (defparameter *wav-file* "/home/karthik/quicklisp/local-projects/signal/music-stereo.wav")

;; (time (defparameter *wav* (load-wav-file *wav-file*)))
;; ;;; ;TODO: chunk-audio-data
;; (time (defparameter *sig* (make-signal-processing (audio-data *wav*)
;;                                                   (num-channels *wav*))))
;; (time (defparameter *mfcc-obj* (make-mfcc *sig*)))
;; (time (defparameter *lift* (apply-liftering-to-dct *mfcc-obj*)))
;; (time (defparameter *log-mean* (get-mean-normalization (log-mel-features *mfcc-obj*))))
;; (time (defparameter *mfcc-mean* (get-mean-normalization *lift*)))
;; (time (defparameter *final* (delta *lift* 2)))




(defparameter *wav* (load-wav-file *wav-file*))
;; (defparameter *snip* (chunk-audio-data (load-wav-file *wav-file*) 0.5))
;; (defparameter *sig* (make-signal-processing *snip*
;;                                             (num-channels *wav*)))
(defparameter *sig* (make-signal-processing (audio-data *wav*)
                                            (num-channels *wav*)))
(defparameter *mfcc-obj* (make-mfcc *sig*))
(defparameter *lift* (apply-liftering-to-dct *mfcc-obj*))
(defparameter *log-mean* (get-mean-normalization (log-mel-features *mfcc-obj*)))
(defparameter *mfcc-mean* (get-mean-normalization *lift*))
(defparameter *final* (delta *lift* 2))
(defparameter *energy* (find-energy (power-spectrum *sig*)))


(aops:sub *lift* 0)
(aops:sub *final* 0)
(aops:sub (log-mel-features *mfcc-obj*) 0)

