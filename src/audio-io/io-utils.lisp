(in-package #:mfcc)


;;; Write buffer function to add the raw data into a source buffer.
(defun write-buffer-with-raw-data (buffer source start end &key (buffer-len 2048))
  "Takes the values of raw buffer and put it in the source buffer."
  (let ((src-len (- (length source) buffer-len))
        (idx 0))
    (iter
      (while (< (+ idx start) end))
      (if (<= (+ idx start) src-len)
          (setf (aref source (+ idx start))
                (aref buffer idx)))
      (incf idx))
    buffer))


(defun fill-buffer (buffer source start end)
  "Function to fill the buffer with source data."
  (let ((src-len (get-row source))
        (buf-len (get-row buffer))
        (idx 0))
    (iter
      (while (and (< (+ idx start) end)
                  (< idx buf-len)))
      (if (>= (+ idx start) src-len)
          (setf (aref buffer idx) 0.0)
          (setf (aref buffer idx) (aref source (+ idx start))))
      (incf idx))
    buffer))


(defun array-to-list (array)
  "Converts the array into list."
  (let* ((dimensions (array-dimensions array))
         (depth (1- (length dimensions)))
         (indices (make-list (1+ depth) :initial-element 0)))
    (labels ((recurse (n)
               (iter
                 (for j below (nth n dimensions))
                 (setf (nth n indices) j)
                 (collect (if (= n depth)
                              (apply #'aref array indices)
                              (recurse (1+ n)))))))
      (recurse 0))))


(defun write-tag (tag stream)
  "Writes a 4-character ASCII tag to the stream. Takes string and convert it to character. Writes the characters into the stream."
  (iter
    (for ch :in-string tag)
    (write-byte (char-code ch) stream)))


(defun write-tag-to-file (filename tag)
  "Writes the tag(string) into the file in byte format."
  (let ((file-path (merge-pathnames filename
                                    *current-directory*)))
    (with-open-file (fstream file-path
                             :direction :output
                             :if-exists :supersede
                             :element-type 'unsigned-byte)
      (write-tag tag fstream))))

#+test
(write-tag-to-file "example.txt" "RIFF")


(defun convert-unsigned-int-to-bytes (unsigned-int byte-count stream)
  "Converts unsigned integer into bytes"
  (iter
    (for n :below (* byte-count 8) :by 8)
    (write-byte (ldb (byte 8 n) unsigned-int) stream)))


(defun convert-signed-int-to-bytes (signed-int byte-count stream)
  "Converts signed integer into bytes"
  (when (< signed-int 0)
    (incf signed-int (expt 2 (* byte-count 8))))
  (iter
    (for i :below (* byte-count 8) :by 8)
    (write-byte (ldb (byte 8 i) signed-int) stream)))


(defun signed-to-unsigned (value size)
  "Return the unsigned representation of a signed byte with a given size.  Size is the number of BYTES."
  (ldb (byte size 0) value))


(defun unsigned-to-signed (value size)
  "Return the signed representation of an unsigned byte with a given size.  Size is the number of BYTES."
  (if (logbitp (1- size) value)
      (dpb value (byte size 0) -1)
      value))

(defun integer-to-bytes (int &key (byte-count 4))
  (let ((bytes '()))
    (dotimes (i byte-count)
      (push (ldb (byte 8 (* i 8)) int)
	    bytes))
    (nreverse bytes)))



(defun bytes-to-integer (byte-array &optional swap-endianness)
  "Converts an array of bytes to an integer.  MUST BE LITTLE-ENDIAN!!"
  (when swap-endianness
    (setf byte-array (reverse byte-array)))
  (let ((int 0))
    (iter
      (for byte :in-sequence byte-array)
      (for i :from 0 :to (length byte-array))
      (for offset = (* i 8))
      (setf (ldb (byte 8 offset) int) byte))
    int))



(defun write-unsigned-int-to-bytes (unsigned-int
                                    stream
                                    &key (byte-count 4))
  "High level function to convert unsigned integer into a byte and write it in the stream."
  (convert-unsigned-int-to-bytes unsigned-int byte-count stream))


(defun write-signed-int-to-bytes (signed-int
                                  stream
                                  &key (byte-count 4))
  "High level function to convert signed integer into a byte and write it in the stream."
  (convert-signed-int-to-bytes signed-int byte-count stream))


;;; Taken from "https://github.com/belambert/cl-asr/blob/master/src/interface/wav-interface.lisp"
(defun int-array-to-byte-array (int-array bits-per-int)
  "Convert a byte array into an int array.  Requires the number of bits per int to be specified. If the number of bits is 16, then each pair of bytes forms a single 16-bit integer."
  (assert (= (mod bits-per-int 8) 0))
  (let* ((bytes-per-int (coerce (/ bits-per-int 8) 'integer))
         (byte-array-length (* (length int-array) bytes-per-int))
         (byte-array (make-array byte-array-length :element-type '(unsigned-byte 8)))
         (int-array-length (length int-array)))
    (iter
      (for int :in-sequence int-array)
      (for int-offset :from 0 :below int-array-length)
      (for byte-list = (integer-to-bytes int :byte-count bytes-per-int))
      (for byte-array-offset1 = (* bytes-per-int int-offset))
      (iter
        (for byte in byte-list)
        (for i from 0 below bytes-per-int)
        (for byte-array-offset2 = (+ byte-array-offset1 i))
        (setf (aref byte-array byte-array-offset2) byte)))
    byte-array))



(defun byte-array-to-int-array (byte-array bits-per-int)
  "Convert a byte array into an int array.  Requires the number of bits per int to be specified. If the number of bits is 16, then each pair of bytes forms a single 16-bit integer."
  (assert (= (mod bits-per-int 8) 0))
  (let* ((bytes-per-int (coerce (/ bits-per-int 8) 'integer))
         (int-array-length (/ (length byte-array) bytes-per-int))
         (int-array (make-array int-array-length
                                :element-type 'single-float)))
    (format t "~20A:~10D~%" "bytes-per-int" bytes-per-int)
    (iter
      (for int-offset from 0 below int-array-length)
      (for byte-offset = (* bytes-per-int int-offset))
      (setf (aref int-array int-offset)
            (coerce (unsigned-to-signed
                     (bytes-to-integer
                      (subseq byte-array byte-offset
                              (+ byte-offset bytes-per-int)))
                      bits-per-int) 'single-float)))
    (format t "~20A: ~10d~%" "int-array shape" (array-dimensions int-array))    
    int-array))



(defun plot-signal (x filename &key (y nil y-supplied-p)
                                 (signal-label ";signal;")
                                 (x-label "x-label")
                                 (y-label "y-label")
                                 (linespace '(0 1000 0 100) linespace-supplied-p)
                                 (title "Auto-generated"))
  "Minimal high level function to interact with VGPLOT."
  (progn
    (if y-supplied-p
        (vgplot:plot x y signal-label)
        (vgplot:plot x signal-label))
    (vgplot:xlabel x-label)
    (vgplot:ylabel y-label)
    (vgplot:title title)
    (when linespace-supplied-p
      (vgplot:axis linespace))
    (vgplot:print-plot (create-file-path filename))
    (vgplot:format-plot t "set terminal ~A" "qt"))
  'SAVED)


(defun create-wave-samples (buffer wave-function frequency time-step)
  "Create a wave signal with wave-function."
  (iter
    (for i :index-of-vector buffer)
    (setf (aref buffer i)
          (funcall wave-function
                   (coerce (* +tau+
                              frequency
                              (* i time-step)) 'single-float))))
    ;; (format t "~&Sampling interval (Ts): ~f" (/ i sample-rate))
    
  buffer)


(defun print-data (samples)
  "Prints the data."
  (iter
    (for index :index-of-sequence samples)
    (format t "~&~A " (aref samples index))))


;;; Save sample data as a normal file.
(defun save-to-file (filename samples)
  "Creates a File with sample data."
  (with-open-file (fstream
                   (create-file-path filename)
                   :direction :output
                   :if-exists :supersede)
    (iter
      (for sample in-sequence samples)
      (format fstream "~&~A" sample))))




(defun chunk-audio-data (audio-obj seconds)
  "Returns the chunk of audio data seconds."
  (let* ((audio-data (audio-data audio-obj))
         (audio-len (length audio-data))
         (sample-rate (sample-rate audio-obj))
         (chunk-len (round (* sample-rate seconds))))
    (if (>= chunk-len audio-len)
        (progn
          (format t "~&Cannot snip the audio; The audio size is less than the required time seconds. Returning the original audio. ~&Original audio: ~A~%" audio-len)
          audio-data)
        (aops:subvec audio-data 0 chunk-len))))

;; (defparameter *snip*
;;   (chunk-audio-data *wav* 1))
;; (defparameter *mfcc-obj-1* (make-mfcc *snip*))
;; (save-to-file "snip.txt" *snip*)



(defun fill-value (dimensions value &key (type 'integer))
  "Fills the value to the dimensions with the specified type."
  (make-array dimensions
              :element-type type
              :initial-element value))

(defun zeros (dimensions &key (type 'integer))
  "Returns zeros of dimensions with the specified type."
  (fill-value dimensions (coerce 0 type) :type type))


;; (defun data-as-2d-array (audio-data)
;;   "doc"
;;   (let* ((audio-len (length audio-data))
;;          (reshape-len (ceiling (/ audio-len 2))))
;;     (if (evenp audio-len)
;;         (return-from data-as-2d-array (aops:reshape audio-data `(,reshape-len 2)))
;;         (progn
;;           (format t "~20A: ~10d~&" "restructure len" reshape-len)
;;           (return-from data-as-2d-array (aops:reshape (concatenate 'vector audio-data #(0.0)) `(,reshape-len 2)))))))


(defun data-as-list-vector (audio-data num-channels)
  (let ((audio-len (length audio-data)))
    (declare (type (simple-array single-float *) audio-data)
             (type fixnum num-channels audio-len))
    (if (evenp audio-len)
        (fill-list-vector audio-data num-channels)
        (fill-list-vector (concatenate 'vector audio-data #(0.0)) num-channels))))


(defun fill-list-vector (audio-data num-channels)
  (let ((audio-len (length audio-data)))
    (declare (type (simple-array single-float *) audio-data)
             (type fixnum num-channels audio-len))
    (iter
      (for i :from 0 :below audio-len :by num-channels)
      (collect (list (aref audio-data i)
                     (aref audio-data (1+ i))) result-type 'vector))))


(defun restructure-data (audio-data num-channels)
  "Reshapes the audio-data wrt num-channels. For mono audio, the data will be returned as it is. But for the stereo audio, the data will be split into right and left channel forming a 2D array."
  (declare (type (simple-array single-float *) audio-data)
           (type fixnum num-channels))
  (when (= num-channels 1)
    (return-from restructure-data audio-data))
  (when (> num-channels 1)
    (return-from restructure-data (data-as-list-vector audio-data num-channels))))


;; (setf a (restructure-data (audio-data *wav*) (num-channels *wav*)))
;; (save-to-file "audio.txt" (audio-data *mfcc-obj-1*))

(defun find-audio-length (audio-data num-channels)
  "Returns the audio length based on the audio type. Stereo audio-data is a 2D array containing left and right channel values. For mono, it is simply 1D array."
  (* (car (array-dimensions audio-data))
     num-channels))


