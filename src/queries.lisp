;;;; queries.lisp

(in-package #:js8call)

;; This is the global counter for @APRSIS messages.
(defvar *sequence* 0)
(defvar *sequence-lock* (bt:make-lock))

(defun get-rig-freq ()
  "Ask JS8Call to get the radio's dial frequency."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "RIG.GET_FREQ")
	  (cons "value" "")))))

(defun set-rig-freq (dial offset)
  "Ask JS8Call to set the radio's dial frequency."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ
		(cons "DIAL" dial)
		(cons "OFFSET" offset))
	  (cons "type" "RIG.SET_FREQ")
	  (cons "value" "")))))

(defun get-callsign ()
  "Ask JS8Call for the configured callsign."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "STATION.GET_CALLSIGN")
	  (cons "value" "")))))

(defun get-grid-square ()
  "Ask JS8Call for the configured grid square."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "STATION.GET_GRID")
	  (cons "value" "")))))

(defun get-info ()
  "Ask JS8Call for the configured info field."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "STATION.GET_INFO")
	  (cons "value" "")))))

(defun set-grid-square (grid)
  "Ask JS8Call to update the configured grid square."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "STATION.SET_GRID")
	  (cons "value" grid)))))

(defun set-info (info)
  "Ask JS8Call to update the configured info field."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "STATION.SET_INFO")
	  (cons "value" info)))))

(defun get-call-activity ()
  "Get the contents of the right white box."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "RX.GET_CALL_ACTIVITY")
	  (cons "value" "")))))

(defun get-call-selected ()
  "ToDo: I don't remember what this does."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "RX.GET_CALL_SELECTED")
	  (cons "value" "")))))

(defun get-band-activity ()
  "Get the contents of the left white box."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "RX.GET_BAND_ACTIVITY")
	  (cons "value" "")))))

(defun get-rx-text ()
  "Get the contents of the yellow window."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "RX.GET_TEXT")
	  (cons "value" "")))))

(defun get-tx-text ()
  "Get the contents of the box below yellow window."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "TX.GET_TEXT")
	  (cons "value" "")))))

(defun set-tx-text (text)
  "Queue the text up for sending, but don't actually send it (ie, you
have to hit <return> on the keyboard to send. Probably not very
useful, but included for completeness."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "TX.SET_TEXT")
	  (cons "value" text)))))

(defun send-text (message)
  "Send the supplied text on the next transmit cycle."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "TX.SEND_MESSAGE")
	  (cons "value" message)))))

(defun get-speed ()
  "Ask JS8Call what speed it's currently configured for."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "MODE.GET_SPEED")
	  (cons "value" "")))))

(defun set-speed (speed)
  "Set the JS8Call modem speed. Note that there's a bug in JS8Call 2.2
where the speed will change as requested, but nothing in the GUI will
correctly reflect that change."
  (let ((spd
	 (cond
	   ((equal :slow speed) 4)
	   ((equal :normal speed) 0)
	   ((equal :fast speed) 1)
	   ((equal :turbo speed) 2)
	   (t speed))))
    (send-message 
     (jsown:to-json
      (list :OBJ
	    (list "params" :OBJ
		  (cons "SPEED" spd))
	    (cons "type" "MODE.SET_SPEED")
	    (cons "value" ""))))))

(defun window-raise ()
  "Raise the JS8Call window to the top."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "WINDOW.RAISE")
	  (cons "value" "")))))

(defun snr? (call)
  "Ask for an SNR from another station."
  (send-text (concatenate 'string call " SNR?")))

(defun grid? (call)
  "Ask for a grid from another station."
  (send-text (concatenate 'string call " GRID?")))

(defun info? (call)
  "Ask for another station,s INFO string."
  (send-text (concatenate 'string call " INFO?")))

(defun status? (call)
  "Ask for another station,s STATUS string."
  (send-text (concatenate 'string call " STATUS?")))

(defun hearing? (call)
  "Ask who another station is hearing."
  (send-text (concatenate 'string call " HEARING?")))

(defun agn? (call)
  "Ask a station to repeat it's last transmission."
  (send-text (concatenate 'string call " AGN?")))

(defun qsl? (call)
  "Ask a station if he heard your last message."
  (send-text (concatenate 'string call " QSL?")))

(defun send-grid-square-to-aprs (&optional grid)
  "Send my grid square to @APRSIS (use the one fetched from JS8Call if
not supplied)."
  (if (or *my-grid* grid)
      (send-text (concatenate 'string "@APRSIS GRID " (if grid grid *my-grid*)))
      (format t "Unknown Grid Square. Unable to send Grid.~%~%")))

(defun send-heartbeat ()
  "Send a heartbeat message."
  (when *my-grid*
    (when (>= (length *my-grid*) 4)
      (send-text
       (concatenate 'string "@HB HEARTBEAT " (subseq *my-grid* 0 4))))))

(defun query-messages (&optional (dest-call "@ALLCALL"))
  "Ask another station (or @ALLCALL) if there are any messages for me."
  (send-text
   (format nil "~A QUERY MSGS" dest-call)))

(defun send-sms (phone message &optional (dest-call "@APRSIS"))
  "Send an SMS message."
  (bt:with-lock-held (*sequence-lock*)
    (send-text
     (format nil "~A CMD :SMSGTE  :@~A ~A{~3,'0D}"
	     dest-call phone message
	     (incf *sequence*)))))

(defun send-email (address message &optional (dest-call "@APRSIS"))
  "Send a short email message."
  (bt:with-lock-held (*sequence-lock*)
    (send-text
     (format nil "~A CMD :EMAIL-2  :~A ~A{~3,'0D}"
	     dest-call address message
	     (incf *sequence*)))))

(defun send-directed-message (dest-call message)
  "Send a directed message to a specific call."
  (send-text
   (format nil "~A MSG ~A"
	   dest-call message)))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
