;;;; frames.lisp

(in-package #:js8call)

; (get-frame-type "RX.DIRECTED" (get-log))
(defun get-frame-type (type frames)
  "Return all received messages of the specified type."
  (remove-if-not
   (lambda (a)
     (equal type (jsown:val a "type")))
   frames))

(defun get-param (thing which)
  "Utility function for make-frame functions."
  (jsown:val (jsown:val thing "params") which))

;; This is the base class that holds some stuff common to all types of
;; frames. We only inherit this, never instantiate it.
(defclass js8-frame ()
  ((frame-type :accessor frame-type :initarg :frame-type :initform nil)
   (frame-value :accessor frame-value :initarg :frame-value :initform nil)
   (frame-id :accessor frame-id :initarg :frame-id :initform nil)
   (frame-timestamp :accessor frame-timestamp :initarg :frame-timestamp :initform nil)
   (frame-raw :accessor frame-raw :initarg :frame-raw :initform nil)))

;; Class for a TX.FRAME
(defclass tx-frame (js8-frame)
  ((frame-tones :accessor frame-tones :initarg :frame-tones :initform nil)))

(defun make-tx-frame (thing)
  "Make a TX.FRAME object from the JSON."
  (make-instance 'tx-frame
		 :frame-type (clean-string (jsown:val thing "type"))
		 :frame-value (if (equal "" (jsown:val thing "value"))
				  nil
				  (string-trim (jsown:val thing "value")))
		 :frame-id (get-param thing "_ID")
		 :frame-timestamp (jsown:val thing "TIMESTAMP")
		 :frame-tones (get-param thing "TONES")
		 :frame-raw (jsown:val thing "RAW")))

(defmethod process-frame ((f tx-frame))
  "Do any necessary processing on a TX.FRAME frame."
  t)

(defmethod frame-print ((f tx-frame))
  "Print a TX.FRAME frame."
  (format t "Type: ~A~%" (frame-type f))
  (format t "Time: ~A~%" (local-time:unix-to-timestamp (frame-timestamp f)))
  (when (frame-value f)
    (format t "Value: ~A~%" (frame-value f)))
  (unless *suppress-id*
    (format t "ID: ~A~%" (frame-id f)))
  (format t "Tones: ~A~%" (frame-tones f))
  (format t "~%"))

;; Class for a STATION.GRID frame.
(defclass station-grid-frame (js8-frame)
  ((frame-grid :accessor frame-grid :initarg :frame-grid :initform nil)))

(defun make-station-grid-frame (thing)
  "Make a STATION.GRID object from the JSON."
  (make-instance 'station-grid-frame
		 :frame-type (clean-string (jsown:val thing "type"))
		 :frame-value (if (equal "" (jsown:val thing "value"))
				  nil
				  (clean-string (jsown:val thing "value")))
		 :frame-id (get-param thing "_ID")
		 :frame-timestamp (jsown:val thing "TIMESTAMP")
		 :frame-grid (if (equal "" (jsown:val thing "value"))
				 nil
				 (clean-string (jsown:val thing "value")))
		 :frame-raw (jsown:val thing "RAW")))

(defmethod process-frame ((f station-grid-frame))
  "Do any necessary processing on a STATION.GRID frame."
  (new-grid *my-call* (frame-grid f) (type-of f))
  (setf *my-grid* (frame-grid f)))

(defmethod frame-print ((f station-grid-frame))
  "Print a STATION.GRID frame."
  (format t "Type: ~A~%" (frame-type f))
  (format t "Time: ~A~%" (local-time:unix-to-timestamp (frame-timestamp f)))
  (unless *suppress-id*
    (format t "ID: ~A~%" (frame-id f)))
  (format t "Grid: ~A~%" (frame-grid f))
  (format t "~%"))

;; Class for a STATION.CALLSIGN frame.
(defclass station-callsign-frame (js8-frame)
  ((frame-call :accessor frame-call :initarg :frame-call :initform nil)))

(defun make-station-callsign-frame (thing)
  "Make a STATION.CALLSIGN object from the JSON."
  (make-instance 'station-callsign-frame
		 :frame-type (clean-string (jsown:val thing "type"))
		 :frame-value (if (equal "" (jsown:val thing "value"))
				  nil
				  (clean-string (jsown:val thing "value")))
		 :frame-id (get-param thing "_ID")
		 :frame-timestamp (jsown:val thing "TIMESTAMP")
		 :frame-call (if (equal "" (jsown:val thing "value"))
				 nil
				 (clean-string (jsown:val thing "value")))
		 :frame-raw (jsown:val thing "RAW")))

(defmethod process-frame ((f station-callsign-frame))
  "Do any necessary processing on a STATION.CALLSIGN frame."
  (setf *my-call* (frame-call f)))

(defmethod frame-print ((f station-callsign-frame))
  "Print a STATION.CALLSIGN frame."
  (format t "Type: ~A~%" (frame-type f))
  (format t "Time: ~A~%" (local-time:unix-to-timestamp (frame-timestamp f)))
  (unless *suppress-id*
    (format t "ID: ~A~%" (frame-id f)))
  (format t "Call: ~A~%" (frame-call f))
  (format t "~%"))

;; Class for a RIG.PTT frame.
(defclass rig-ptt-frame (js8-frame)
  ((frame-ptt :accessor frame-ptt :initarg :frame-ptt :initform nil)))

(defun make-rig-ptt-frame (thing)
  "Make a RIG.PTT object from the JSON."
  (make-instance 'rig-ptt-frame
		 :frame-type (clean-string (jsown:val thing "type"))
		 :frame-value (if (equal "" (jsown:val thing "value"))
				  nil
				  (clean-string (jsown:val thing "value")))
		 :frame-id (get-param thing "_ID")
		 :frame-timestamp (jsown:val thing "TIMESTAMP")
		 :frame-ptt (clean-string (get-param thing "PTT"))
		 :frame-raw (jsown:val thing "RAW")))

(defmethod process-frame ((f rig-ptt-frame))
  "Do any necessary processing on a RIG.PTT frame."
  t)

(defmethod frame-print ((f rig-ptt-frame))
  "Print a RIG.PTT frame."
  (format t "Type: ~A~%" (frame-type f))
  (format t "Time: ~A~%" (local-time:unix-to-timestamp (frame-timestamp f)))
  (when (frame-value f)
    (format t "Value: ~A~%" (frame-value f)))
  (unless *suppress-id*
    (format t "ID: ~A~%" (frame-id f)))
  (format t "PTT: ~A~%" (frame-ptt f))
  (format t "~%"))

;; Base class for a received frame (not instantiated).
(defclass rx-frame (js8-frame)
  ((frame-dial-freq :accessor frame-dial-freq :initarg :frame-dial-freq :initform nil)
   (frame-freq :accessor frame-freq :initarg :frame-freq :initform nil)
   (frame-offset :accessor frame-offset :initarg :frame-offset :initform nil)))

;; Class for a RIG.FREQ frame.
(defclass rig-freq-frame (rx-frame)
  ())

(defun make-rig-freq-frame (thing)
  "Make a RIG.FREQ object from the JSON."
  (make-instance 'station-status-frame
		 :frame-type (clean-string (jsown:val thing "type"))
		 :frame-value (if (equal "" (jsown:val thing "value"))
				  nil
				  (clean-string (jsown:val thing "value")))
		 :frame-id (get-param thing "_ID")
		 :frame-timestamp (jsown:val thing "TIMESTAMP")
		 :frame-dial-freq (/ (get-param thing "DIAL") 1000.0)
		 :frame-freq (/ (get-param thing "FREQ") 1000.0)
		 :frame-offset (/ (get-param thing "OFFSET") 1000.0)
		 :frame-raw (jsown:val thing "RAW")))

(defmethod process-frame ((f rig-freq-frame))
  "Do any necessary processing on a RIG.FREQ frame."
  t)

(defmethod frame-print ((f rig-freq-frame))
  "Print a STATION.STATUS frame."
  (format t "Type: ~A~%" (frame-type f))
  (format t "Time: ~A~%" (local-time:unix-to-timestamp (frame-timestamp f)))
  (when (frame-value f)
    (format t "Value: ~A~%" (frame-value f)))
  (unless *suppress-id*
    (format t "ID: ~A~%" (frame-id f)))
  (format t "Dial Freq: ~,3F khz~%" (frame-dial-freq f))
  (format t "Offset: ~A hz~%" (round (* 1000 (frame-offset f))))
  (format t "TX Freq: ~,3F khz~%" (frame-freq f))
  (format t "~%"))

;; Class for a STATION.STATUS frame.
(defclass station-status-frame (rx-frame)
  ((frame-selected :accessor frame-selected :initarg :frame-selected :initform nil)
   (frame-speed :accessor frame-speed :initarg :frame-speed :initform nil)))
   
(defun make-station-status-frame (thing)
  "Make a STATION.STATUS object from the JSON."
  (make-instance 'station-status-frame
		 :frame-type (clean-string (jsown:val thing "type"))
		 :frame-value (if (equal "" (jsown:val thing "value"))
				  nil
				  (clean-string (jsown:val thing "value")))
		 :frame-id (get-param thing "_ID")
		 :frame-timestamp (jsown:val thing "TIMESTAMP")
		 :frame-dial-freq (/ (get-param thing "DIAL") 1000.0)
		 :frame-freq (/ (get-param thing "FREQ") 1000.0)
		 :frame-offset (/ (get-param thing "OFFSET") 1000.0)
		 :frame-selected (if (equal "" (get-param thing "SELECTED"))
				     nil
				     (clean-string (get-param thing "SELECTED")))
		 :frame-speed (get-param thing "SPEED")
		 :frame-raw (jsown:val thing "RAW")))

(defmethod process-frame ((f station-status-frame))
  "Do any necessary processing on a STATION.STATUS frame."
  t)

(defmethod frame-print ((f station-status-frame))
  "Print a STATION.STATUS frame."
  (format t "Type: ~A~%" (frame-type f))
  (format t "Time: ~A~%" (local-time:unix-to-timestamp (frame-timestamp f)))
  (when (frame-value f)
    (format t "Value: ~A~%" (frame-value f)))
  (unless *suppress-id*
    (format t "ID: ~A~%" (frame-id f)))
  (format t "Dial Freq: ~,3F khz~%" (frame-dial-freq f))
  (format t "Offset: ~A hz~%" (round (* 1000 (frame-offset f))))
  (format t "TX Freq: ~,3F khz~%" (frame-freq f))
  (when (frame-selected f)
    (format t "Selected: ~A~%" (frame-selected f)))
  (format t "Speed: ~A~%" (nth (frame-speed f) *speed*))
  (format t "~%"))

;; Class for a RX.SPOT frame.
(defclass rx-spot-frame (rx-frame)
  ((frame-call :accessor frame-call :initarg :frame-call :initform nil)
   (frame-grid :accessor frame-grid :initarg :frame-grid :initform nil)
   (frame-snr :accessor frame-snr :initarg :frame-snr :initform nil)))

(defun make-rx-spot-frame (thing)
  "Make a RX.SPOT object from the JSON."
  (make-instance 'rx-spot-frame
		 :frame-type (clean-string (jsown:val thing "type"))
		 :frame-value (if (equal "" (jsown:val thing "value"))
				  nil
				  (clean-string (jsown:val thing "value")))
		 :frame-id (get-param thing "_ID")
		 :frame-timestamp (jsown:val thing "TIMESTAMP")
		 :frame-dial-freq (/ (get-param thing "DIAL") 1000.0)
		 :frame-freq (/ (get-param thing "FREQ") 1000.0)
		 :frame-offset (/ (get-param thing "OFFSET") 1000.0)
		 :frame-call (clean-string (get-param thing "CALL"))
		 :frame-grid (clean-string (get-param thing "GRID"))
		 :frame-snr (get-param thing "SNR")
		 :frame-raw (jsown:val thing "RAW")))

(defmethod process-frame ((f rx-spot-frame))
  "Do any necessary processing on a RX.SPOT frame."
  (new-grid (frame-call f) (frame-grid f) (type-of f))
  (new-qso (frame-call f) *my-call* (frame-snr f) nil (frame-freq f) nil (type-of f))
  t)

(defmethod frame-print ((f rx-spot-frame))
  "Print a RX.SPOT frame."
  (unless *suppress-heartbeat*
    (format t "Type: ~A~%" (frame-type f))
    (format t "Time: ~A~%" (local-time:unix-to-timestamp (frame-timestamp f)))
    (when (frame-value f)
      (format t "Value: ~A~%" (frame-value f)))
    (unless *suppress-id*
      (format t "ID: ~A~%" (frame-id f)))
    (format t "Dial Freq: ~,3F khz~%" (frame-dial-freq f))
    (format t "Offset: ~A hz~%" (round (* 1000 (frame-offset f))))
    (format t "TX Freq: ~,3F khz~%" (frame-freq f))
    (format t "Call: ~A~%" (frame-call f))
    (format t "Grid: ~A~%" (frame-grid f))
    (if *print-snr-correctly*
	(format t "SNR: ~A:1~%" (frame-snr f))
	(format t "SNR: ~A~%" (frame-snr f)))
    (format t "~%")))

;; Class for a RX.ACTIVITY frame.
(defclass rx-activity-frame (rx-frame)
  ((frame-snr :accessor frame-snr :initarg :frame-snr :initform nil)
   (frame-speed :accessor frame-speed :initarg :frame-speed :initform nil)
   (frame-drift :accessor frame-drift :initarg :frame-drift :initform nil)))

(defun make-rx-activity-frame (thing)
  (make-instance 'rx-activity-frame
		 :frame-type (clean-string (jsown:val thing "type"))
		 :frame-value (if (equal "" (jsown:val thing "value"))
				  nil
				  (clean-string (jsown:val thing "value")))
		 :frame-id (get-param thing "_ID")
		 :frame-timestamp (jsown:val thing "TIMESTAMP")
		 :frame-dial-freq (/ (get-param thing "DIAL") 1000.0)
		 :frame-freq (/ (get-param thing "FREQ") 1000.0)
		 :frame-offset (/ (get-param thing "OFFSET") 1000.0)
		 :frame-snr (get-param thing "SNR")
		 :frame-speed (get-param thing "SPEED")
		 :frame-drift (round (* (get-param thing "TDRIFT") 1000.0))
		 :frame-raw (jsown:val thing "RAW")))

(defmethod process-frame ((f rx-activity-frame))
  "Do any necessary processing on a RX.ACTIVITY frame."
  t)

(defmethod frame-print ((f rx-activity-frame))
  "Print a RX.ACTIVITY frame."
  (unless *suppress-activity*
    (format t "Type: ~A~%" (frame-type f))
    (format t "Time: ~A~%" (local-time:unix-to-timestamp (frame-timestamp f)))
    (when (frame-value f)
      (format t "Value: ~A~%" (frame-value f)))
    (unless *suppress-id*
      (format t "ID: ~A~%" (frame-id f)))
    (format t "Dial Freq: ~,3F khz~%" (frame-dial-freq f))
    (format t "Offset: ~A hz~%" (round (* 1000 (frame-offset f))))
    (format t "TX Freq: ~,3F khz~%" (frame-freq f))
    (if *print-snr-correctly*
	(format t "SNR: ~A:1~%" (frame-snr f))
	(format t "SNR: ~A~%" (frame-snr f)))
    (format t "Speed: ~A~%" (nth (frame-speed f) *speed*))
    (format t "Drift: ~A ms~%" (frame-drift f))
    (format t "~%")))

;; Class for a RX.DIRECTED frame.
(defclass rx-directed-frame (rx-frame)
  ((frame-cmd :accessor frame-cmd :initarg :frame-cmd :initform nil)
   (frame-extra :accessor frame-extra :initarg :frame-extra :initform nil)
   (frame-from :accessor frame-from :initarg :frame-from :initform nil)
   (frame-grid :accessor frame-grid :initarg :frame-grid :initform nil)
   (frame-snr :accessor frame-snr :initarg :frame-snr :initform nil)
   (frame-overheard-snr :accessor frame-overheard-snr :initarg :frame-overheard-snr :initform nil)
   (frame-speed :accessor frame-speed :initarg :frame-speed :initform nil)
   (frame-drift :accessor frame-drift :initarg :frame-drift :initform nil)
   (frame-text :accessor frame-text :initarg :frame-text :initform nil)
   (frame-to :accessor frame-to :initarg :frame-to :initform nil)))

(defun make-rx-directed-frame (thing)
  "Make a RX.DIRECTED object from the JSON."
  (make-instance 'rx-directed-frame
		 :frame-type (clean-string (jsown:val thing "type"))
		 :frame-value (if (equal "" (jsown:val thing "value"))
				  nil
				  (clean-string (jsown:val thing "value")))
		 :frame-id (get-param thing "_ID")
		 :frame-timestamp (jsown:val thing "TIMESTAMP")
		 :frame-dial-freq (/ (get-param thing "DIAL") 1000.0)
		 :frame-freq (/ (get-param thing "FREQ") 1000.0)
		 :frame-offset (/ (get-param thing "OFFSET") 1000.0)
		 :frame-snr (get-param thing "SNR")
		 :frame-overheard-snr (if (or (equal "SNR" (clean-string (get-param thing "CMD")))
					      (equal "HEARTBEAT SNR" (clean-string (get-param thing "CMD"))))
					  (parse-integer (clean-string (get-param thing "EXTRA")))
					  nil)
		 :frame-speed (get-param thing "SPEED")
		 :frame-drift (round (* (get-param thing "TDRIFT") 1000.0))
		 :frame-grid (if (equal "GRID" (clean-string (get-param thing "CMD")))
				 (nth 3
				      (split-sequence:split-sequence
				       #\Space (clean-string (get-param thing "TEXT"))))
				 (if (equal "" (get-param thing "GRID"))
				     nil
				     (clean-string (get-param thing "GRID"))))
		 :frame-cmd (clean-string (get-param thing "CMD"))
		 :frame-extra (if (equal "" (get-param thing "EXTRA"))
				     nil
				     (clean-string (get-param thing "EXTRA")))
		 :frame-from (clean-string (get-param thing "FROM"))
		 :frame-to (clean-string (get-param thing "TO"))
		 :frame-text (clean-string (get-param thing "TEXT"))
		 :frame-raw (jsown:val thing "RAW")))

(defmethod process-frame ((f rx-directed-frame))
  "Do any necessary processing on a RX.DIRECTED frame."
  (new-grid (frame-from f) (frame-grid f) (type-of f))
  (new-qso (frame-from f) (frame-to f) (frame-snr f) (frame-overheard-snr f) (frame-freq f) (frame-speed f) (type-of f))
  t)

(defmethod frame-print ((f rx-directed-frame))
  "Print a RX.DIRECTED frame."
  (unless (and *suppress-heartbeat*
	       (or (equal "HEARTBEAT" (frame-cmd f))
		   (equal "HEARTBEAT SNR" (frame-cmd f))))
    (format t "Type: ~A~%" (frame-type f))
    (format t "Time: ~A~%" (local-time:unix-to-timestamp (frame-timestamp f)))
;;; 'value' and 'text' are the same with an RX.DIRECTED frame, so no
;;; point in printing both.
;;;    (when (frame-value f)
;;;      (format t "Value: ~A~%" (frame-value f)))
    (unless *suppress-id*
      (format t "ID: ~A~%" (frame-id f)))
    (format t "Dial Freq: ~,3F khz~%" (frame-dial-freq f))
    (format t "Offset: ~A hz~%" (round (* 1000 (frame-offset f))))
    (format t "TX Freq: ~,3F khz~%" (frame-freq f))
    (if *print-snr-correctly*
	(format t "SNR: ~A:1~%" (frame-snr f))
	(format t "SNR: ~A~%" (frame-snr f)))
    (when (frame-overheard-snr f)
      (format t "Their SNR: ~A~%" (frame-overheard-snr f)))
    (format t "Speed: ~A~%" (nth (frame-speed f) *speed*))
    (format t "Drift: ~A ms~%" (frame-drift f))
    (when (frame-grid f)
      (format t "Grid: ~A~%" (frame-grid f)))
    (format t "Cmd: ~A~%" (frame-cmd f))
    (when (frame-extra f)
      (format t "Extra: ~A~%" (frame-extra f)))
    (format t "From: ~A~%" (frame-from f))
    (format t "To: ~A~%" (frame-to f))
    (format t "Text: ~A~%" (frame-text f))
    (format t "~%")))

(defun thing-to-obj (thing)
  "Turn the provided JSON into the appropriate object."
  (let ((type (jsown:val thing "type"))
	(frame nil))
    (cond
      ((equal "STATION.CALLSIGN" type)
       (setf frame (make-station-callsign-frame thing)))
      ((equal "STATION.GRID" type)
       (setf frame (make-station-grid-frame thing)))
      ((equal "TX.FRAME" type)
       (setf frame (make-tx-frame thing)))
      ((equal "STATION.STATUS" type)
       (setf frame (make-station-status-frame thing)))
      ((equal "RIG.FREQ" type)
       (setf frame (make-rig-freq-frame thing)))
      ((equal "RIG.PTT" type)
       (setf frame (make-rig-ptt-frame thing)))
      ((equal "RX.DIRECTED" type)
       (setf frame (make-rx-directed-frame thing)))
      ((equal "RX.SPOT" type)
       (setf frame (make-rx-spot-frame thing)))
      ((equal "RX.ACTIVITY" type)
       (setf frame (make-rx-activity-frame thing)))
      (t
       (format t "~%----------------~%~A~%----------------~%~%" thing)))
    (process-frame frame)
    frame))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
