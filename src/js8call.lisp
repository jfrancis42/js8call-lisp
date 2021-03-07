;;;; js8call.lisp

(in-package #:js8call)

(setf *read-default-float-format* 'double-float)

;; Chatty settings.
(defvar *verbose* nil)
(defvar *suppress-id* t)
(defvar *suppress-activity* nil)
(defvar *suppress-heartbeat* nil)
(defvar *suppress-cq* nil)
(defvar *print-snr-correctly* nil)

;; Globals for my station.
(defvar *my-grid* nil)
(defvar *my-call* nil)
(defvar *js8-host* "localhost")
(defvar *js8-port* 2442)

;; Stuff
(defvar *qso-lock* (bt:make-lock))
(defvar *log-qso* nil)
(defvar *grid-lock* (bt:make-lock))
(defvar *log-grid* nil)

(defun loud ()
  (setf *suppress-activity* nil)
  (setf *suppress-heartbeat* nil)
  (setf *suppress-cq* nil))
  
(defun quiet ()
  (setf *suppress-activity* t)
  (setf *suppress-heartbeat* t)
  (setf *suppress-cq* t))
  
;; This class holds information about observed transmissions between
;; one station and another. It implies nothing with respect to whether
;; or not bi-directional communications was achieved.
(defclass qso ()
  ((qso-timestamp :accessor qso-timestamp :initarg :qso-timestamp :initform nil)
   (qso-from :accessor qso-from :initarg :qso-from :initform nil)
   (qso-to :accessor qso-to :initarg :qso-to :initform nil)
   (qso-snr :accessor qso-snr :initarg :qso-snr :initform nil)
   (qso-overheard-snr :accessor qso-overheard-snr :initarg :qso-overheard-snr :initform nil)
   (qso-freq :accessor qso-freq :initarg :qso-freq :initform nil)
   (qso-his-speed :accessor qso-his-speed :initarg :qso-his-speed :initform nil)
   (qso-text :accessor qso-text :initarg :qso-text :initform nil)
   (qso-source :accessor qso-source :initarg :qso-source :initform nil)))

(defmethod pp ((q qso))
  "Print a QSO object."
  (format t "Time: ~A~%" (local-time:unix-to-timestamp (qso-timestamp q)))
  (format t "From: ~A~%" (qso-from q))
  (format t "To: ~A~%" (qso-to q))
  (if *print-snr-correctly*
      (format t "SNR: ~A:1~%" (qso-snr q))
      (format t "SNR: ~A~%" (qso-snr q)))
  (when (qso-overheard-snr q)
    (if *print-snr-correctly*
	(format t "SNR Reported: ~A:1~%" (qso-overheard-snr q))
	(format t "SNR Reported: ~A~%" (qso-overheard-snr q))))
  (format t "TX Freq: ~,3F khz~%" (qso-freq q))
  (when (qso-his-speed q)
    (format t "Speed: ~A~%" (nth (qso-his-speed q) *speed*)))
  (format t "Text: ~A~%" (qso-text q))
  (format t "Source: ~A~%" (qso-source q))
  (format t "~%"))

(defmethod complete? ((q qso))
  (not (search *missing* (qso-text q))))

(defun make-qso (from to snr overheard-snr freq his-speed text frame-type)
  "Make a QSO object from the relevant info."
  (make-instance 'qso
		 :qso-timestamp (local-time:timestamp-to-unix (local-time:now))
		 :qso-from from
		 :qso-to to
		 :qso-snr snr
		 :qso-overheard-snr overheard-snr
		 :qso-freq freq
		 :qso-his-speed his-speed
		 :qso-text text
		 :qso-source frame-type))

(defun new-qso (from to snr overheard-snr freq his-speed text frame-type)
  "Add a QSO to the list."
  (bt:with-lock-held (*qso-lock*)
    (setf *log-qso* (cons (make-qso from to snr overheard-snr freq his-speed text frame-type) *log-qso*))))

(defmethod serialize ((q qso))
  "Serialize a qso object to JSON."
  (let ((json nil))
    (setf (jsown:val json "timestamp") (qso-timestamp q))
    (setf (jsown:val json "from") (qso-from q))
    (setf (jsown:val json "to") (qso-to q))
    (setf (jsown:val json "snr") (qso-snr q))
    (when (qso-overheard-snr q)
      (setf (jsown:val json "overheard-snr") (qso-overheard-snr q)))
    (setf (jsown:val json "freq") (qso-freq q))
    (setf (jsown:val json "his-speed") (qso-his-speed q))
    (setf (jsown:val json "text") (qso-text q))
    (setf (jsown:val json "source") (qso-source q))
    (jsown:to-json json)))

(defun find-qso (c1 c2)
  "Find all messages between two stations."
  (let ((call-1 (clean (string-upcase c1)))
	(call-2 (clean (string-upcase c2))))
    (remove-if-not
     (lambda (q)
       (or (and (equal call-1 (qso-from q)) (equal call-2 (qso-to q)))
	   (and (equal call-1 (qso-to q)) (equal call-2 (qso-from q)))))
     *log-qso*)))

;; This class holds information about what time and what grid a
;; specific callsign reported itself to be in.
(defclass grid ()
  ((grid-timestamp :accessor grid-timestamp :initarg :grid-timestamp :initform nil)
   (grid-call :accessor grid-call :initarg :grid-call :initform nil)
   (grid-grid :accessor grid-grid :initarg :grid-grid :initform nil)
   (grid-source :accessor grid-source :initarg :grid-source :initform nil)))

(defun make-grid (call grid source)
  "Make a grid object from the relevant info."
  (make-instance 'grid
		 :grid-timestamp (local-time:timestamp-to-unix (local-time:now))
		 :grid-call call
		 :grid-grid grid
		 :grid-source source))

(defun new-grid (call grid source)
  "Add a grid square to the list."
  (when (and (not (equal "" grid))
	     call grid)
    (bt:with-lock-held (*grid-lock*)
      (setf *log-grid* (cons (make-grid call grid source) *log-grid*)))))

(defmethod serialize ((g grid))
  "Serialize a grid object to JSON."
  (let ((json nil))
    (setf (jsown:val json "timestamp") (grid-timestamp g))
    (setf (jsown:val json "call") (grid-call g))
    (setf (jsown:val json "source") (grid-source g))
    (jsown:to-json json)))

(defun get-grid (call)
  (let ((found (remove-if-not (lambda (g) (equal call (grid-call g))) *log-grid*)))
    (when found
      (grid-grid
       (first
	(sort found (lambda (a b) (> (grid-timestamp a) (grid-timestamp b)))))))))

(defun get-distance (from-grid to-grid)
  (af:rad-to-sm
   (af:calc-distance (af:from-maidenhead from-grid)
		     (af:from-maidenhead (get-grid to-grid)))))

(defun get-bearing (from-grid to-grid)
  (af:deg-to-cardinal-course
   (af:calc-gc-bearing (af:from-maidenhead from-grid)
		       (af:from-maidenhead (get-grid to-grid)))))

(defun matches ()
  "Broken, maybe."
  (let ((matches nil))
    (mapcar
     (lambda (a)
       (mapcar
	(lambda (b)
	  (when (and (<= (abs (- (qso-timestamp a) (qso-timestamp b))) 180)
		     (equal (qso-from a) (qso-to b))
		     (equal (qso-to a) (qso-from b)))
	    (setf matches (cons (cons a b) matches))))
	*log-qso*))
     *log-qso*)
    matches))

(defun matches-2 ()
  "Broken, maybe."
  (let ((matches nil))
    (mapcar
     (lambda (a)
       (mapcar
	(lambda (b)
	  (when (and (equal (jsown:val (jsown:val a "params") "FROM")
			    (jsown:val (jsown:val b "params") "TO"))
		     (equal (jsown:val (jsown:val a "params") "TO")
			    (jsown:val (jsown:val b "params") "FROM")))
	    (setf matches (cons (cons a b) matches))))
	(get-frame-type "RX.DIRECTED" (get-log "../log.json"))))
     (get-frame-type "RX.DIRECTED" (get-log "../log.json")))
    matches))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
