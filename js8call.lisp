;;;; js8call.lisp

(in-package #:js8call)

(setf *read-default-float-format* 'double-float)

(defvar *listener-thread* nil)
(defvar *talker-thread* nil)
(defvar *tx-queue-lock* (bt:make-lock))
(defvar *rx-queue-lock* (bt:make-lock))
(defvar *all-heard-lock* (bt:make-lock))
(defvar *heard-lock* (bt:make-lock))
(defvar *verbose* nil)
(defvar *socket* nil)

(defvar *sequence* 0)
(defvar *sequence-lock* (bt:make-lock))

(defvar *call* nil)
(defvar *grid* nil)
(defvar *dial* nil)
(defvar *offset* nil)
(defvar *ping* nil)
(defvar *flag* nil)
(defvar *heard* nil)
(defvar *all-heard* nil)
(defvar *grids* (make-hash-table :test #'equal))

(defvar *tx-q* (jeff:make-queue))
(defvar *rx-q* (jeff:make-queue))

(defun fix-time (time)
  (round (/ time 1000)))

(defun js8-time (time)
  (unix-to-timestamp (fix-time time)))

(defun call-bearing (src-call dst-call)
  (let ((src (call-loc src-call))
	(dst (call-loc dst-call)))
    (when (and src dst)
      (af:rad-to-deg
       (af:calc-gc-bearing src dst)))))

(defun call-distance (src-call dst-call)
  (let ((src (call-loc src-call))
	(dst (call-loc dst-call)))
    (when (and src dst)
      (af:rad-to-sm
       (af:calc-distance src dst)))))

(defun call-loc (call)
  (when (gethash call *grids*)
    (af:from-maidenhead (gethash call *grids*))))

(defun write-heard (&optional (file "heard.json"))
  (bt:with-lock-held (*heard-lock*)
    (with-open-file
	(f file :direction :output :if-exists :supersede)
      (format f "~A~%" (json:encode-json-to-string *heard*)))))

(defun validate-string (str)
  (let ((stuff (split-sequence #\Space str)))
    (when (>= (length stuff) 7)
      (when (and (> (length (nth 4 stuff)) 1)
		 (> (length (nth 5 stuff)) 0))
	(when (equal ":" (subseq (nth 4 stuff) (- (length (nth 4 stuff)) 1)))
	  (let ((tod (nth 0 stuff))
		(offset (parse-integer (subseq (nth 2 stuff) 1 (- (length (nth 2 stuff)) 1))))
		(from (subseq (nth 4 stuff) 0 (- (length (nth 4 stuff)) 1)))
		(to (nth 5 stuff))
		(cmd (nth 6 stuff)))
	    (unless (or (equal "HB" to)
			(equal "CQ" to))
	      (cons from to))))))))

(defun process-entry-test (entry)
  (let* ((type (cdr-assoc "type" entry))
	 (params (rest (cdr-assoc "params" entry)))
	 (value (cdr-assoc "value" entry))
    	 (timestamp (cdr-assoc :timestamp entry)))
    (cond
      ((equal "STATION.CALLSIGN" type)
       (setf *call* (clean-string value)))
      ((equal "STATION.GRID" type)
       (setf *grid* (string-or-nil value))
       (setf (gethash *call* *grids*) (clean-string *grid*)))
      ((equal "STATION.GRID" type)
       (setf *grid* (string-or-nil value))
       (setf (gethash *call* *grids*) (clean-string *grid*)))
      ((equal "RX.SPOT" type)
       (when (not (equal "" (cdr-assoc "GRID" params)))
	 (setf (gethash (cdr-assoc "CALL" params) *grids*)
	       (string-or-nil (clean-string (cdr-assoc "GRID" params))))))
      ((equal "RX.DIRECTED" type)
       (when (not (equal "" (cdr-assoc "GRID" params)))
	 (setf (gethash (cdr-assoc "CALL" params) *grids*)
	       (clean-string (cdr-assoc "GRID" params))))))))

(defun process-entry (entry)
  (let* ((type (cdr-assoc "type" entry))
	 (params (rest (cdr-assoc "params" entry)))
	 (value (cdr-assoc "value" entry))
    	 (timestamp (cdr-assoc :timestamp entry)))
    (when (cdr-assoc "DIAL" params)
      (setf *dial* (cdr-assoc "DIAL" params)))
    (cond
      ((equal "RIG.FREQ" type)
       (setf *dial* (cdr-assoc "DIAL" params))
       (setf *offset* (cdr-assoc "OFFSET" params))
       (setf *flag* t))
      ((equal "STATION.CALLSIGN" type)
       (setf *call* (clean-string value)))
      ((equal "STATION.GRID" type)
       (setf *grid* (string-or-nil value))
       (setf (gethash *call* *grids*) (clean-string *grid*)))
      ((equal "RX.SPOT" type)
       (when (not (equal "" (cdr-assoc "GRID" params)))
	 (setf (gethash (cdr-assoc "CALL" params) *grids*)
	       (string-or-nil (clean-string (cdr-assoc "GRID" params)))))
       (bt:with-lock-held (*heard-lock*)
	 (setf *heard*
	       (cons
		(list (cons :timestamp timestamp)
		      (cons :from (cdr-assoc "CALL" params))
		      (cons :to *call*)
		      (cons :snr (cdr-assoc "SNR" params))
		      (cons :grid (cdr-assoc "GRID" params))
		      (cons :freq (+ (cdr-assoc "DIAL" params)
				     (cdr-assoc "OFFSET" params))))
		*heard*))))
      ((equal "RX.DIRECTED" type)
       (when (not (equal "" (cdr-assoc "GRID" params)))
	 (setf (gethash (cdr-assoc "CALL" params) *grids*)
	       (clean-string (cdr-assoc "GRID" params)))))
;;      ((equal "RX.DIRECTED" type)
;;       (when (not (equal "" (cdr-assoc "GRID" params)))
;;	 (setf (gethash (cdr-assoc "CALL" params) *grids*)
;;	       (clean-string (cdr-assoc "GRID" params))))
;;       (bt:with-lock-held (*heard-lock*)
;;	 (setf *heard*
;;	       (cons
;;		(list (cons :timestamp (fix-time (cdr-assoc "UTC" params)))
;;		      (cons :from (cdr-assoc "FROM" params))
;;		      (cons :to (cdr-assoc "TO" params))
;;		      (cons :snr (cdr-assoc "SNR" params))
;;		      (cons :grid (cdr-assoc "GRID" params))
;;		      (cons :freq (+ *dial* *offset*))
;;		      (cons :cmd (cdr-assoc "CMD" params))
;;		      (cons :extra (cdr-assoc "EXTRA" params)))
;;		*heard*))))
      ((equal "RX.TEXT" type)
       (bt:with-lock-held (*all-heard-lock*)
	 (setf *all-heard* (make-hash-table :test #'equal))
	 (mapcar
	  (lambda (n)
	    (setf (gethash (car n) *all-heard*)
		  (remove-duplicate-strings
		   (mapcar
		    (lambda (m)
		      (string-trim (list #\>) m))
		    (cons (cdr n) (gethash (car n) *all-heard*))))))
	  (mapcar #'validate-string
		  (remove-if-not
		   #'validate-string
		   (remove-if
		    (lambda (a)
		      (equal "" a))
		    (split-sequence #\Newline value)))))))
      ((equal "PING" type)
       (setf *ping* timestamp))
      )))

(defun create-listener (socket)
  (loop ;repeat 10 do
	(usocket:wait-for-input socket)
	(bt:with-lock-held (*rx-queue-lock*)
	  (jeff:enqueue
	   (cons
	    (cons
	     :timestamp
	     (timestamp-to-unix
	      (now)))
	    (rest (jsown:parse (read-line (usocket:socket-stream socket))))) *rx-q*))))

(defun create-talker (socket)
  (loop 
   (unless (jeff:queue-empty-p *tx-q*)
     (bt:with-lock-held (*tx-queue-lock*)
       (format (usocket:socket-stream socket) "~A~C~C"
	       (jeff:dequeue *tx-q*)
	       #\return #\linefeed)
       (force-output (usocket:socket-stream socket))))
   (sleep 0.5)))

(defun start-server (&optional (host "localhost") (port 2442))
  (setf *socket* (usocket:socket-connect host port :element-type 'character))
  (setf *listener-thread* (bt:make-thread
			   (lambda () (create-listener *socket*))
			   :name "js8-listener"))
  (setf *talker-thread* (bt:make-thread
			 (lambda () (create-talker *socket*))
			 :name "js8-talker"))
  (get-callsign)
  (get-grid-square))

(defun kill-server ()
  (bt:destroy-thread *listener-thread*)
  (bt:destroy-thread *talker-thread*)
  (usocket:socket-close *socket*))

(defun send-message (message)
  (jeff:enqueue message *tx-q*))

(defun dump-rx (&optional ignore-ping)
  (remove-if (lambda (n) (equal (cdr-assoc :type n) (if ignore-ping "PING" (gensym))))
	     (loop while (not (jeff:queue-empty-p *rx-q*))
		collect (jeff:dequeue *rx-q*))))

(defun get-rig-freq () ; works
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "RIG.GET_FREQ")
	  (cons "value" "")))))

(defun set-rig-freq (dial offset) ; works
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ
		(cons "DIAL" dial)
		(cons "OFFSET" offset))
	  (cons "type" "RIG.SET_FREQ")
	  (cons "value" "")))))

(defun get-callsign () ; works
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "STATION.GET_CALLSIGN")
	  (cons "value" "")))))

(defun get-grid-square () ; works
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "STATION.GET_GRID")
	  (cons "value" "")))))

(defun get-info () ; works
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "STATION.GET_INFO")
	  (cons "value" "")))))

(defun set-grid-square (grid) ; works
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "STATION.SET_GRID")
	  (cons "value" grid)))))

(defun set-info (info) ; updated, untested
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "STATION.SET_INFO")
	  (cons "value" info)))))

(defun get-call-activity () ; works
  "right white box"
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "RX.GET_CALL_ACTIVITY")
	  (cons "value" "")))))

(defun get-call-selected () ; works
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "RX.GET_CALL_SELECTED")
	  (cons "value" "")))))

(defun get-band-activity () ; works
  "left white box"
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "RX.GET_BAND_ACTIVITY")
	  (cons "value" "")))))

(defun get-rx-text () ; works
  "yellow window"
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "RX.GET_TEXT")
	  (cons "value" "")))))

(defun get-tx-text () ; works
  "below yellow window"
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "TX.GET_TEXT")
	  (cons "value" "")))))

(defun set-tx-text (text) ; works
  "Queue the text up for sending, but don't actually send it (ie, you
have to hit <return> on the keyboard to send."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "TX.SET_TEXT")
	  (cons "value" text)))))

(defun send-text (message) ; works
  "Send the supplied text on the next transmit cycle."
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "TX.SEND_MESSAGE")
	  (cons "value" message)))))

(defun get-speed () ; works
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "MODE.GET_SPEED")
	  (cons "value" "")))))

(defun set-speed (speed) ; works
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

(defun window-raise () ; not tested
  (send-message
   (jsown:to-json
    (list :OBJ
	  (list "params" :OBJ)
	  (cons "type" "WINDOW.RAISE")
	  (cons "value" "")))))

(defun send-grid-square-to-aprs (&optional grid) ; works
  ;; @APRSIS GRID CN87WU31IB
  (if (or *grid* grid)
      (send-text (concatenate 'string "@APRSIS GRID " (if grid grid *grid*)))
      (format t "Unknown Grid Square. Unable to send Grid.~%~%")))

(defun send-spot () ; works
  ;; @HB HEARTBEAT CN87
  (when *grid*
    (when (>= (length *grid*) 4)
      (send-text
       (concatenate 'string "@HB HEARTBEAT " (subseq *grid* 0 4))))))

(defun send-sms (phone message)
  ;; @APRSIS CMD :SMSGTE  :@2069659825 TEST SMS MESSAGE{001}
  (bt:with-lock-held (*sequence-lock*)
;;    (set-tx-text
    (send-text
     (format nil "@APRSIS CMD :SMSGTE  :@~A ~A{~3,'0D'}"
	     phone message
	     (incf *sequence*)))))

(defun send-email (email message)
  ;; @APRSIS CMD :EMAIL-2  :JEFF@GRITCH.ORG TEST MESSAGE{001}
  (bt:with-lock-held (*sequence-lock*)
;;    (set-tx-text
    (send-text
     (format nil "@APRSIS CMD :EMAIL-2  :~A ~A{~3,'0D'}"
	     email message
	     (incf *sequence*)))))

(defun send-directed-message (dest-call message)
  ;; K0FBS MSG TEST TEST TEST
;;  (set-tx-text
  (send-text
   (format nil "~A MSG ~A"
	   dest-call message)))

(defun print-entry (entry)
  (process-entry entry)
  (let* ((type (cdr-assoc "type" entry))
	 (params (rest (cdr-assoc "params" entry)))
	 (value (cdr-assoc "value" entry))
	 (timestamp (cdr-assoc :timestamp entry)))
    (cond
      ((equal "RIG.FREQ" type)
       (format t "----- RIG.FREQ -----~%Time: ~A~%Dial: ~A~%Offset: ~A~%Freq: ~A~%~%"
	       (unix-to-timestamp timestamp)
	       (cdr-assoc "DIAL" params)
	       (cdr-assoc "OFFSET" params)
	       (+ (cdr-assoc "DIAL" params)
		  (cdr-assoc "OFFSET" params))))
      ((equal "STATION.INFO" type)
       (format t "----- STATION.INFO -----~%Time: ~A~%Text: ~A~%~%"
	       (unix-to-timestamp timestamp)
	       value))
      ((equal "STATION.STATUS" type)
       (format t "----- STATION.STATUS -----~%Time: ~A~%Dial: ~A hz~%Offset: ~A hz~%Freq: ~A hz~%Speed: ~A~%~%"
	       (unix-to-timestamp timestamp)
	       (cdr-assoc "DIAL" params)
	       (cdr-assoc "OFFSET" params)
	       (+ (cdr-assoc "DIAL" params)
		  (cdr-assoc "OFFSET" params))
	       (cdr-assoc "SPEED" params)))
      ((equal "RX.ACTIVITY" type)
       (format t "----- RX.ACTIVITY -----~%Time: ~A~%Dial: ~A hz~%Offset: ~A hz~%Freq: ~A hz~%SNR: ~A:1~%Speed: ~A~%Drift: ~D ms~%Value: ~A~%~%"
	       (unix-to-timestamp timestamp)
	       (cdr-assoc "DIAL" params)
	       (cdr-assoc "OFFSET" params)
	       (+ (cdr-assoc "DIAL" params)
		  (cdr-assoc "OFFSET" params))
	       (cdr-assoc "SNR" params)
	       (cdr-assoc "SPEED" params)
	       (round (* 1000.0 (cdr-assoc "TDRIFT" params)))
	       value))
      ((equal "STATION.CALLSIGN" type)
       (format t "----- STATION.CALLSIGN -----~%Time: ~A~%Call: ~A~%~%"
	       (unix-to-timestamp timestamp)
	       value))
      ((equal "RX.CALL_ACTIVITY" type)
       (format t "----- RX.CALL_ACTIVITY -----~%Time: ~A~%~A~%~%"
	       (unix-to-timestamp timestamp)
	       params))
      ((equal "RX.CALL_SELECTED" type)
       (format t "----- RX.CALL_SELECTED -----~%Time: ~A~%Params: ~A~%Value: ~A~%~%"
	       (unix-to-timestamp timestamp)
	       params
	       value))
      ((equal "RX.BAND_ACTIVITY" type)
       (format t "----- RX.BAND_ACTIVITY -----~%Time: ~A~%~A~%~%"
	       (unix-to-timestamp timestamp)
	       params))
      ((equal "RX.TEXT" type)
       (format t "----- RX.TEXT -----~%Time: ~A~%"
	       (unix-to-timestamp timestamp))
       )
;;       (with-open-file
;;	   (file "/home/jfrancis/Dropbox/n0gq_heard.dot" :direction :output :if-exists :supersede)
;;	 (format file "digraph {~%")
;;	 (bt:with-lock-held (*all-heard-lock*)
;;	   ;; output a list of who heard who, excluding me
;;	   (mapcar
;;	    (lambda (a)
;;	      (mapcar
;;	       (lambda (b)
;;		 (when (and (not (equal *call* a))
;;			    (not (equal *call* b)))
;;		   (format file "  \"~A\" -> \"~A\";~%" a b)))
;;	       (gethash a *all-heard*))
;;	      (format t "~A: ~A~%"
;;		      a
;;		      (english-join (gethash a *all-heard*))))
;;	    (hash-table-keys *all-heard*)))
;;	 (let ((heard-me (remove nil
;;				 (mapcar
;;				  (lambda (a)
;;				    (when (member *call* (gethash a *all-heard*) :test #'equal) a))
;;				  (hash-table-keys *all-heard*)))))
;;	   ;; output a list of people who heard me
;;	   (mapcar
;;	    (lambda (a)
;;	      (format file "  \"~A\" -> \"~A\";~%" *call* a))
;;	    heard-me)
;;	   ;; output a list of people who I heard who somebody else heard, as well
;;	   (mapcar
;;	    (lambda (a)
;;	      (format file "  \"~A\" -> \"~A\";~%" a *call*))
;;	    (intersection (gethash *call* *all-heard*)
;;			  (remove *call*
;;				  (remove-duplicate-strings
;;				   (alexandria:flatten
;;				    (mapcar
;;				     (lambda (a)
;;				       (gethash a *all-heard*))
;;				     (remove *call* (hash-table-keys *all-heard*) :test #'equal))))
;;				  :test #'equal)
;;			  :test #'equal))	      
;;	   ;; output a list of people who I heard who also heard me
;;	   (mapcar
;;	    (lambda (a)
;;	      (format file "  \"~A\" -> \"~A\";~%" a *call*))
;;	    (remove-if-not (lambda (b) (member b heard-me :test #'equal)) (gethash *call* *all-heard*))))
;;	 (format file "}~%"))
;;       (format t "~%"))
      ((equal "MODE.SPEED" type)
       (format t "----- MODE.SPEED -----~%Time: ~A~%Speed: ~A~%~%"
	       (unix-to-timestamp timestamp)
	       (cdr-assoc "SPEED" params)))
      ((equal "STATION.GRID" type)
       (format t "----- STATION.GRID -----~%Time: ~A~%Grid: ~A~%~%"
	       (unix-to-timestamp timestamp)
	       value))
      ((equal "TX.TEXT" type)
       (format t "----- TX.TEXT -----~%Time: ~A~%Text: ~A~%~%"
	       (unix-to-timestamp timestamp)
	       value))
      ((equal "RX.DIRECTED" type)
;;       (if (equal "HEARTBEAT SNR" (cdr-assoc "CMD" params))
	   (format t "----- RX.DIRECTED -----~%Time: ~A~%Cmd: ~A~%From: ~A~%To: ~A~%SNR (~A hearing ~A): ~A:1~%SNR (~A hearing ~A): ~A:1~%Grid: ~A~%Text: ~A~%Dist: ~,1Fmi~%Dir: ~,1F (~A)~%~%"
		   (js8-time (cdr-assoc "UTC" params))
		   (cdr-assoc "CMD" params)
		   (cdr-assoc "FROM" params)
		   (cdr-assoc "TO" params)
;;		   (cdr-assoc "TO" params) (cdr-assoc "FROM" params) (cdr-assoc "SNR" params)
		   *call* (cdr-assoc "FROM" params) (cdr-assoc "SNR" params)
		   (cdr-assoc "FROM" params) (cdr-assoc "TO" params) (parse-integer
								      (cdr-assoc "EXTRA" params) :junk-allowed t)
		   (cdr-assoc "GRID" params)
		   (cdr-assoc "TEXT" params)
		   (if (and (gethash (cdr-assoc "FROM" params) *grids*)
			    (gethash (cdr-assoc "TO" params) *grids*))
		       (call-distance (cdr-assoc "FROM" params) (cdr-assoc "TO" params))
		       "")
		   (if (and (gethash (cdr-assoc "FROM" params) *grids*)
			    (gethash (cdr-assoc "TO" params) *grids*))
		       (call-bearing (cdr-assoc "FROM" params) (cdr-assoc "TO" params))
		       "")
		   (if (and (gethash (cdr-assoc "FROM" params) *grids*)
			    (gethash (cdr-assoc "TO" params) *grids*))
		       (af:deg-to-cardinal-course
			(call-bearing (cdr-assoc "FROM" params) (cdr-assoc "TO" params)))
		       "")))
;;	   (format t "----- RX.DIRECTED -----~%Time: ~A~%Cmd: ~A~%From: ~A~%To: ~A~%SNR (~A hearing ~A): ~A:1~%Grid: ~A~%Extra: ~A~%Text: ~A~%Dist: ~,1Fmi~%Dir: ~,1F (~A)~%~%"
;;		   (js8-time (cdr-assoc "UTC" params))
;;		   (cdr-assoc "CMD" params)
;;		   (cdr-assoc "FROM" params)
;;		   (cdr-assoc "TO" params)
;;		   (cdr-assoc "TO" params) (cdr-assoc "FROM" params) (cdr-assoc "SNR" params)
;;		   (cdr-assoc "GRID" params)
;;		   (cdr-assoc "EXTRA" params)
;;		   (cdr-assoc "TEXT" params)
;;		   (if (and (gethash (cdr-assoc "FROM" params) *grids*)
;;			    (gethash (cdr-assoc "TO" params) *grids*))
;;		       (call-distance (cdr-assoc "FROM" params) (cdr-assoc "TO" params))
;;		       "")
;;		   (if (and (gethash (cdr-assoc "FROM" params) *grids*)
;;			    (gethash (cdr-assoc "TO" params) *grids*))
;;		       (call-bearing (cdr-assoc "FROM" params) (cdr-assoc "TO" params))
;;		       "")
;;		   (if (and (gethash (cdr-assoc "FROM" params) *grids*)
;;			    (gethash (cdr-assoc "TO" params) *grids*))
;;		       (af:deg-to-cardinal-course
;;			(call-bearing (cdr-assoc "FROM" params) (cdr-assoc "TO" params)))
;;		       "")))
      ((equal "TX.FRAME" type)
       (format t "----- TX.FRAME -----~%Time: ~A~%Tones: ~A~%~%"
	       (unix-to-timestamp timestamp)
	       (cdr-assoc "TONES" params)))
      ((equal "RIG.PTT" type)
       (format t "----- RIG.PTT -----~%Time: ~A~%State: ~A~%~%"
	       (unix-to-timestamp timestamp)
	       value))
      ((equal "RX.SPOT" type)
       (format t "----- RX.SPOT -----~%Time: ~A~%Call: ~A~%SNR (~A hearing ~A): ~A:1~%Grid: ~A~%Dial: ~A hz~%Offset: ~A hz~%Freq: ~A hz~%Dist: ~,1Fmi~%Dir: ~,1F (~A)~%~%"
	       (unix-to-timestamp timestamp)
	       (cdr-assoc "CALL" params)
	       *call* (cdr-assoc "CALL" params) (cdr-assoc "SNR" params)
	       (cdr-assoc "GRID" params)
	       (cdr-assoc "DIAL" params)
	       (cdr-assoc "OFFSET" params)
	       (+ (cdr-assoc "DIAL" params)
		  (cdr-assoc "OFFSET" params))
	       (if (gethash (cdr-assoc "CALL" params) *grids*)
		   (call-distance *call* (cdr-assoc "CALL" params))
		   "")
	       (if (gethash (cdr-assoc "CALL" params) *grids*)
		   (call-bearing *call* (cdr-assoc "CALL" params))
		   "")
	       (if (gethash (cdr-assoc "CALL" params) *grids*)
		   (af:deg-to-cardinal-course
		    (call-bearing *call* (cdr-assoc "CALL" params)))
		   "")))
      ((equal "PING" type)
       t)
;;       (format t "----- PING -----~%"))
      (t
       (format t "----- UNKNOWN -----~%Time: ~A~%~A~%"
	       (unix-to-timestamp timestamp)
	       entry)))))

(defun watch-traffic (&optional (forever nil))
  (if forever
      (loop
	 (if (jeff:queue-empty-p *rx-q*)
	     (sleep 0.5)
	     (print-entry (jeff:dequeue *rx-q*))))
      (mapcar #'print-entry (jeff:queue-items *rx-q*)))
  t)

(defun watch-js8 (&optional (host "hoss") (port 2442))
  (let ((socket (usocket:socket-connect host port :element-type 'character)))
    (unwind-protect 
	 (progn
;	   (format (usocket:socket-stream socket) "~A~C~C"
;		   (jsown:to-json
;		    (list :OBJ
;			  (list "params" :OBJ)
;			  (cons "type" "TX.SET_TEXT")
;			  (cons "value" "blah blah blah")))
;;	   (format (usocket:socket-stream socket) "~A~C~C"
;;		   (jsown:to-json
;;		    (list :OBJ
;;			  (list "params" :OBJ)
;;			  (cons "type" "TX.SEND_MESSAGE")
;;			  (cons "value" "@APRSIS GRID DM79PI95FI")))
;		    (list :OBJ
;			  (list "params" :OBJ)
;			  (cons "type" "RIG.GET_FREQ")
;			  (cons "value" "")))
;		    (list :OBJ (list "params" :OBJ) (cons "type" "MODE.GET_SPEED") (cons "value" "")))
;;		   #\return #\linefeed)
;;	   (force-output (usocket:socket-stream socket))
	   (loop repeat 10 do
;;		 (usocket:wait-for-input socket :timeout 1)
		 (format t "~A~%" (read-line (usocket:socket-stream socket)))))
      (usocket:socket-close socket))))

;; (send-spot t t t)
;; (send-grid-square-to-aprs)
;; (get-rx-text)
;; (set-rig-freq 7078000 1500)
;; (get-rig-freq)

;; (ql:quickload :cl-store)
;; (cl-store:store *rx-q* "~/Dropbox/src/lisp/js8call/rxq.09152020.store")
;; (setf *rx-q* (cl-store:restore "~/Dropbox/src/lisp/js8call/rxq.09152020.store"))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
