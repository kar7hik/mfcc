(in-package #:mfcc)


;;; Wave Class
(defclass wave-signal ()
  ((duration :initarg :duration
             :initform *seconds*
             :accessor duration
             :type 'single-float)
   (sample-rate :initarg :sample-rate
                :initform *sample-rate*
                :accessor sample-rate
                :type 'double-float)
   (num-channels :initarg :num-channels
                 :accessor num-channels
                 :initform *num-channels*
                 :type 'integer)
   (sample-interval :initarg :sample-interval
                    :initform *sample-interval*
                    :accessor sample-interval
                    :type 'single-float)
   (sample-points :initarg :sample-points
                  :initform 0
                  :accessor sample-points
                  :type 'integer)
   (audio-buffer-size :initform +audio-buffer-size+
                      :reader audio-buffer-size
                      :type 'integer))
  (:documentation "Wave class - Audio Signal"))


;;;  Wave Header
(defclass wave-header (wave-signal)
  ((chunk-id :accessor chunk-id
             :initarg :chunk-id
             :initform "RIFF"
             :type 'string)
   (chunk-size :accessor chunk-size
               :initarg :chunk-size
               :initform 0)
   (riff-format :accessor riff-format
                :initform "WAVE"
                :initarg :riff-format
                :type 'string)

   ;; fmt subchunk:
   (subchunk1-id :reader subchunk1-id
                 :initform "fmt ")
   (subchunk1-size :reader subchunk1-size
                   :initform 16
                   :type 'integer)
   (audio-format :reader audio-format
                 :initform 1)
   (byte-rate :accessor byte-rate
              :initarg :byte-rate
              :initform 0d0)
   (block-align :accessor block-align
                :initarg :block-align)
   (bits-per-sample :accessor bits-per-sample
                    :initarg :bits-per-sample
                    :initform 16
                    :type 'integer)

   ;; data subchunk
   (subchunk2-id :reader subchunk2-id
                 :initform "data"
                 :type 'string)
   (audio-sample-size :accessor audio-sample-size
                      :initarg :audio-sample-size
                      :initform 0
                      :type 'integer)
   (subchunk2-size :accessor subchunk2-size
                   :initarg :subchunk2-size))
  (:documentation "Wave Header."))


;;; Constructor
(defun make-wave-header (sample-size &key (sample-rate *sample-rate*)
                                       (num-channels *num-channels*))
  (make-instance 'wave-header :audio-sample-size sample-size
                              :sample-rate sample-rate
                              :num-channels num-channels))


;;; Initialization
(defmethod initialize-instance :after ((header wave-header) &key)
  (setf (byte-rate header)
        (* (sample-rate header)
           (num-channels header)
           (/ (bits-per-sample header) 8)))

  (setf (block-align header)
        (* (num-channels header)
           (/ (bits-per-sample header) 8)))

  (setf (subchunk2-size header)
        (* (audio-sample-size header)
           (num-channels header)
           (/ (bits-per-sample header) 8)))

  (setf (chunk-size header)
        (+ (subchunk2-size header)
           36)))


;;;; Audio signal from Microphone. Inherits from wave-signal.
(defclass audio-from-mic (wave-signal)
  ()
  (:documentation "Real time audio signal from Microphone."))


;;;; Audio from a file. Inherits from wave-signal.
(defclass audio-from-file (wave-header) 
  ((audio-data :initarg :audio-data
               :accessor audio-data)
   (audio-length :initarg :audio-length
                 :accessor audio-length))

  (:documentation "Audio signal from a file."))



(defun make-audio-from-file (&key duration sample-rate bits-per-sample audio-data audio-length num-channels)
  "Constructor function for the class audio from file."
  (format t "~20A: ~10d~%" "sample audio-data" (array-dimensions audio-data))
  (make-instance 'audio-from-file :duration duration
                                  :sample-rate sample-rate
                                  :bits-per-sample bits-per-sample
                                  :audio-data audio-data
                                  :audio-length audio-length
                                  :num-channels num-channels))


;;; Wave created from math function. Inherits from wave-signal.
(defclass wave-from-function (wave-signal)
  ((wave-function :initform 'sin
                  :initarg :wave-function
                  :accessor wave-function)
   (frequency :initarg :frequency
              :initform *frequency*
              :accessor frequency
              :type 'double-float))
  (:documentation "Wave object created using a function."))


;;; Initialization
(defmethod initialize-instance :after ((wave wave-from-function) &key)
  (setf (sample-interval wave) (/ 1.0 (sample-rate wave)))
  (setf (sample-points wave) (round (/ (duration wave)
                                       (sample-interval wave)))))

(defmethod initialize-instance :after ((wave audio-from-mic) &key)
  (setf (sample-interval wave) (/ 1.0 (sample-rate wave)))
  (setf (sample-points wave) (round (/ (duration wave)
                                       (sample-interval wave)))))

(defmethod initialize-instance :after ((wave audio-from-file) &key)
  (setf (sample-interval wave) (/ 1.0 (sample-rate wave)))
  (setf (sample-points wave) (round (/ (duration wave)
                                       (sample-interval wave)))))


(defun audio-from-audio-stream (audio source-buffer buffer-len &key (idx 0))
  "Creates audio object. Stores the data into source buffer."
  (with-audio
    (with-default-audio-stream (astream
                                (num-channels audio)
                                (num-channels audio)
                                :sample-format :float
                                :sample-rate (sample-rate audio)
                                :frames-per-buffer (audio-buffer-size audio))
      (format t "~& <<< Starting of Recording >>> ~%")
      (iter
        (while (< idx (array-total-size source-buffer)))
        (let ((buf (read-stream astream)))
          (write-buffer-with-raw-data buf
                                      source-buffer
                                      idx
                                      (+ idx buffer-len)
                                      :buffer-len buffer-len)
          (incf idx buffer-len)))
      (format t "~& <<< End of Recording. >>> ~%"))))


;;; Record audio
(defun record-audio (&key (record-time *seconds*) (save-to-file nil)
                       (audio-filename nil) (plot-data nil)
                       (plot-filename nil)
                       (dft-analysis nil))
  "Records audio using a Microphone."
  (let* ((audio (make-instance 'audio-from-mic
                               :duration record-time))
         (source-buffer-length (round (* (num-channels audio)
                                         (duration audio)
                                         (sample-rate audio))))
         (idx 0)
         (buf-len (* (audio-buffer-size audio)
                     (num-channels audio)))
         (source-buffer (make-array source-buffer-length
                                    :element-type 'single-float
                                    :initial-element 0.0)))
    (format t "~&Audio buffer size (from record audio): ~A" source-buffer-length)
    (audio-from-audio-stream audio
                             source-buffer
                             buf-len
                             :idx idx)
    (when save-to-file
      ;; (save-to-file audio-filename source-buffer)
      (save-as-wav-file source-buffer audio-filename))
    (when plot-data
      (plot-signal source-buffer plot-filename :x-label "time"
                                               :y-label "amplitude"
                                               :title "Recorded audio"
                                               :signal-label "audio"))
    (when dft-analysis
      source-buffer)))



#+test
(record-audio :record-time 2
              :save-to-file t
              :audio-filename "new-mic.wav"
              :plot-data nil
              :plot-filename "record-plot.png")


(defun play-wav-audio (filename)
  "Function to play WAV file."
  (let* ((wav-file (load-wav-file filename :verbose t))
         (num-channels (num-channels wav-file))
         (total-audio-data-size (length (audio-data wav-file)))
         (idx 0)
         (buffer-len (* (audio-buffer-size wav-file) num-channels))
         (buffer (make-array buffer-len
                             :element-type 'single-float
                             :initial-element 0.0)) 
         (audio-source-buffer (coerce (flatten-list (coerce (audio-data wav-file) 'list)) 'vector))
         (divisor (expt 2 (1- (bits-per-sample wav-file)))))
    (setf audio-source-buffer
          (map 'vector (lambda (x)
                         (coerce (/ x divisor)
                                 'single-float))
               audio-source-buffer))
    (with-audio
      (with-default-audio-stream (astream
                                  num-channels
                                  num-channels
                                  :sample-format :float
                                  :sample-rate (sample-rate wav-file)
                                  :frames-per-buffer (audio-buffer-size wav-file))
        (format t "~& <<< Starting Playback >>> ~%")
        (format t "~& ~20A: ~20A ~%" "buffer-len" buffer-len)
        (format t "~& ~20A: ~20A ~%" "audio-size" total-audio-data-size)
        (format t "~& ~20A: ~20A ~%" "num-channels" num-channels)
        (iter
          (while (< idx total-audio-data-size))
          (fill-buffer buffer
                       audio-source-buffer
                       idx
                       (+ idx buffer-len))
          (write-stream astream buffer)
          (incf idx buffer-len))))))


;;; Wave Generate - User API
(defun generate-wave-data (wave &key (with-time t)
                                  (data-size +audio-buffer-size+))
  (let* ((buffer-size nil)
         (buffer nil))
    (when with-time
      (setf buffer-size (round (sample-points wave)))
      (setf buffer (make-array buffer-size :element-type 'single-float
                                           :initial-element 0.0))
      (format t "~&Buffer Size: ~A" buffer-size)
      (create-wave-samples buffer
                           (wave-function wave)
                           (frequency wave)
                           (sample-interval wave)))
    (unless with-time
      (setf buffer (arange 0 (duration wave) :step (/ 1 (sample-rate wave))))
      (setf buffer-size (length buffer))
      (format t "~&Buffer Size: ~A" (array-total-size buffer))
      (create-wave-samples buffer
                           (wave-function wave)
                           (frequency wave)
                           (sample-interval wave)))
    buffer))


(defun create-wave (wave &key (plot-data nil) (plot-filename)
                           (save-to-file nil) (filename nil))
  (let ((buffer (generate-wave-data wave)))
    (when plot-data
      (plot-signal buffer plot-filename))
    (when save-to-file
      (save-as-wav-file buffer filename))))


;;; For testing
#+test
(defparameter *wave* (make-instance 'wave-from-function
                                    :duration 1.0
                                    :frequency 5
                                    :num-channels *num-channels*))

#+test
(create-wave *wave*
             :plot-data t
             :plot-filename "sine-example.png"
             :save-to-file t
             :filename "low-freq.wav")
