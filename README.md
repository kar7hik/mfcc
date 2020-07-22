# Implementation of Mel Frequency Cepstrum Coefficient

[MFCC](https://www.wikiwand.com/en/Mel-frequency_cepstrum)  are the most often used features for Automatic Speech Recognition (ASR). This project is a pure common-lisp implementation of MFCC. For more details and understand the steps involved in the computation of MFCC, refer the [doc](./doc/) folder.


## Usage:

```
(defparameter *wav-file* "./data/music-mono-1.wav")
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

;; => #(84.32953643798828d0 2.320774516956553d0 -35.99634746398786d0 5.854659999539559d0 -9.431935471432663d0 9.418806709697838d0 5.991028051621737d0 12.55219582362723d0 -8.993686128896886d0 -29.59149764341289d0 -18.555555851435216d0 1.50505131483078d0 22.858585376021495d0)

(aops:sub *final* 0)

;; => #(-0.2552299499511719d0 0.6977165031679372d0 -0.09201786919631445d0 2.545779231948922d0 -2.7268834033920015d0 2.06742170401215d0 -3.5643606642114314d0 4.3841496471528565d0 -1.2499408321360161d0 3.7898237565504505d0 -2.156757703371111d0 1.671492236852646d0 -3.8990957484272046d0)

(aops:sub (log-mel-features *mfcc-obj*) 0)

;; => #(12.401644278013046d0 12.325183782025274d0 9.894037918469072d0
  11.87436536355766d0 12.463401597782262d0 13.376237316823566d0
  13.757091339832925d0 11.96858341994982d0 11.816220889334238d0
  11.227166376406652d0 10.129551382254183d0 17.615552923859074d0
  18.078857938546864d0 14.815736271901748d0 14.427964849640757d0
  12.84195386880488d0 13.704143199789389d0 16.635016462483716d0
  14.089993157373893d0 14.462835384938893d0 14.876040837236925d0
  15.794063628793452d0 14.653002044886977d0 15.082592624698782d0
  15.532243769161365d0 14.737328080790803d0 13.913581207693381d0
  13.802102444960566d0 13.896096925050701d0 13.747262284968054d0
  14.433355726798574d0 13.389115891141108d0 12.872882908056825d0
  12.06174263434067d0 12.350199690286363d0 11.429605881258349d0
  11.241366028260343d0 10.990932261487938d0 10.338422068953724d0
  10.299368768592627d0)

```

## License

AGPL

