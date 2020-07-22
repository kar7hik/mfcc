;;;; mfcc.asd

(asdf:defsystem #:mfcc
  :description "Mel Cepstrum Frequency Coefficients"
  :author "S. Karthikkumar <karthikkumar.s@protonmail.com>"
  :license  "AGPL"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:iterate
               #:petalisp
               #:lparallel
               #:cl-portaudio
               #:vgplot
               #:napa-fft3
               #:array-operations
               #:dct)
  
   :components ((:module "src"
                :components ((:file "package")
                             (:file "global")
                             (:file "utils")
                             (:module signal-processing
                              :serial t
                              :components ((:file "signal-processing")
                                           (:file "signal-utils")))
                             (:module audio-io
                              :serial t
                              :components ((:file "io-utils")
                                           (:file "feature-extraction-helper")
                                           (:file "audio-io")
                                           (:file "wav")
                                           (:file "mfcc-helper")
                                           (:file "feature-extraction")))))))
