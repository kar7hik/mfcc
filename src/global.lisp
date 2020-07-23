(in-package #:mfcc)

;;; Audio Processing
;;; Constants:
(defconstant +pi+ (coerce pi 'single-float)
  "Pi value coerced into single float.")
(defconstant +tau+ (* 2 +pi+)
  "2-PI Value")
(defconstant +audio-buffer-size+ 1024
  "The number of samples in the audio buffer.")


;;; Global Variables
(defparameter *sample-rate* 44100d0
  "Default audio sample rate.")
(defparameter *seconds* 1
  "Default time period")
(defparameter *sample-interval* 1
  "Default time interval.")
(defparameter *num-channels* 2
  "Default number of channels.")
(defparameter *frequency* 440
  "Default tone frequency.")

(defparameter *num-worker-threads* 8
  "Number of worker threads for lparallel.")


(defparameter *project-name* "mfcc/")
(defparameter *data-directory* "data/")
(defparameter *local-working-directory* (merge-pathnames *project-name*
                                                         "quicklisp/local-projects/"))

(defparameter *current-directory* (merge-pathnames *local-working-directory*
                                                   (user-homedir-pathname)))
