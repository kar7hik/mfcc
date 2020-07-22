(in-package #:mfcc)

(defun load-wav-file (filename &key (verbose t))
  "Read a wav audio file. See http://www.sonicspot.com/guide/wavefiles.html.  Or: https://ccrma.stanford.edu/courses/422/projects/WaveFormat/"
  (when verbose (format t "Reading WAV file: ~A~%" filename))
  (let ((bytes nil))
    (with-open-file (f filename :direction :input :element-type '(unsigned-byte 8))
      (setf bytes (make-array (file-length f) :element-type '(unsigned-byte 8)))
      (read-sequence bytes f))
    (assert (equal (map 'string 'code-char (subseq bytes 0 4)) "RIFF"))
    (assert (equal (map 'string 'code-char (subseq bytes 8 12)) "WAVE"))
    (assert (equal (map 'string 'code-char (subseq bytes 12 16)) "fmt "))
    (let ((subchunk2-size (- (bytes-to-integer (subseq bytes 4 8)) 36))
	  (subchunk1-size (bytes-to-integer (subseq bytes 16 20)))
          ;; 1=PCM, linear quantization
	  (audio-format (bytes-to-integer (subseq bytes 20 22)))
          ;; number of channels
	  (channels (bytes-to-integer (subseq bytes 22 24)))
          ;; Sample rate in Hz
	  (sample-rate (bytes-to-integer (subseq bytes 24 28)))
          ;; == SampleRate * NumChannels * BitsPerSample/8
	  (byte-rate (bytes-to-integer (subseq bytes 28 32)))
          ;;BlockAlign == NumChannels * BitsPerSample/8
          ;; The number of bytes for one sample including all channels.
	  (block-align (bytes-to-integer (subseq bytes 32 34)))
          ;; 8 bits = 8, 16 bits = 16, etc.
	  (bits-per-sample (bytes-to-integer (subseq bytes 34 36))) 
	  ;; If it's not PCM there may be more here....  Specifically:.. 2   ExtraParamSize   if PCM, then doesn't exist;  X   ExtraParams      space for extra parameters
	  (data-size (bytes-to-integer (subseq bytes 40 44)))
	  (data (subseq bytes 44)))
      (declare (ignore subchunk2-size))
      (when verbose
	(format t "~20A: ~10d~%" "Audio format" audio-format)
	(format t "~20A: ~10d~%" "Channels" channels)
	(format t "~20A: ~10d~%" "Sample rate" sample-rate)
        (format t "~20A: ~10d~%" "Data length" (length data))
	(format t "~20A: ~10d~%" "Byte rate" byte-rate)
	(format t "~20A: ~10d~%" "Bits per sample" bits-per-sample)
	(force-output t))
      (assert (= subchunk1-size 16)) ;; 16 for PCM...
      (assert (= audio-format 1))    ;; o/w it's compressed....
      (assert (= byte-rate (* sample-rate channels (/ bits-per-sample 8))))
      (assert (= block-align (* channels (/ bits-per-sample 8))))
      ;; Beginning of SubChunk2...
      (assert (equal (map 'string 'code-char (subseq bytes 36 40)) "data"))
      ;; If it's not 8-bit data, make the array into a *sample* array, not a *byte* array
      (format t "~20A: ~10d~%" "xx-audio-data" (array-dimensions data))
      (setf data (byte-array-to-int-array data bits-per-sample))
      ;; (let ((divisor (expt 2 (1- bits-per-sample))))
      ;;  (setf data (compute (alpha #'/ data divisor)))
      ;;  (setf data (map 'vector (lambda (x) (coerce (/ x divisor) 'single-float)) data))
      ;;         )
      (setf data (restructure-data data channels))
      (let* ((duration 0.0))
        (when (= channels 1)
          (setf duration (float (/ (length data) byte-rate)))
          (format t "~20A: ~10d~&" "duration" duration))
        (when (> channels 1)
          (setf duration
                (float (/ (car (array-dimensions data)) byte-rate)))
          (format t "~20A: ~10d~&" "(2) duration" duration))
        (setf data-size (find-audio-length data channels))
        (make-audio-from-file :duration duration
                              :sample-rate (coerce sample-rate 'double-float)
                              :bits-per-sample bits-per-sample
                              :audio-data data
                              :audio-length data-size
                              :num-channels channels)))))

;; #+test
;; (defparameter *wav-file* (load-wav-file "/home/karthik/quicklisp/local-projects/signal/low-freq.wav" :verbose t)
;;   "Loading wav file.")
;;; #+test
;;(save-to-file "audio.txt" (audio-data *wav-file*))

;;; Save sample data as Wav file.
(defun save-as-wav-file (sample-data filename &key (verbose nil)
                                                (sample-rate *sample-rate*)
                                                (num-channels *num-channels*))
  "Creates a Wav file"
  (let* ((header (make-wave-header (length sample-data)
                                   :sample-rate  sample-rate
                                   :num-channels num-channels))
         (file (create-file-path filename))
         ;; Sample multiplier value taken from
         ;; https://www3.nd.edu/~dthain/courses/cse20211/fall2013/wavfile/example.c
         (bytes nil)
         (sample-multiplier 12000)) ; 3200
    (when verbose
      (format t "~&sample-size: ~A~&sample rate: ~A" (audio-sample-size header)
              (sample-rate header)))
    (with-open-file (audio-stream
                     file
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)

      (write-tag (chunk-id header) audio-stream)
      (write-signed-int-to-bytes (chunk-size header)
                                 audio-stream
                                 :byte-count 4)
      (write-tag (riff-format header) audio-stream)
      (write-tag (subchunk1-id header) audio-stream)
      (write-unsigned-int-to-bytes (subchunk1-size header)
                                 audio-stream
                                 :byte-count 4)
      (write-unsigned-int-to-bytes (audio-format header)
                                 audio-stream
                                 :byte-count 2)
      (write-unsigned-int-to-bytes (num-channels header)
                                 audio-stream
                                 :byte-count 2)
      (write-unsigned-int-to-bytes (round (sample-rate header))
                                 audio-stream
                                 :byte-count 4)
      (write-unsigned-int-to-bytes (round (byte-rate header))
                                 audio-stream
                                 :byte-count 4)
      (write-unsigned-int-to-bytes (block-align header)
                                 audio-stream
                                 :byte-count 2)
      (write-unsigned-int-to-bytes (bits-per-sample header)
                                 audio-stream
                                 :byte-count 2)
      (write-tag (subchunk2-id header) audio-stream)
      (write-unsigned-int-to-bytes (subchunk2-size header)
                                 audio-stream
                                 :byte-count 4)
      (setf sample-data (map 'vector (lambda (x)
                                       (round (* x sample-multiplier)))
                             sample-data))

      (setf bytes (int-array-to-byte-array sample-data
                                           (bits-per-sample header)))
      ;; Finally write the samples to the file
      (write-sequence bytes audio-stream)))
  'SUCCESS)
