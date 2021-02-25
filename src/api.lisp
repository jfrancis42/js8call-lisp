;;;; api.lisp

;; Queues and locks for tx/rx queues.
(defvar *tx-queue-lock* (bt:make-lock))
(defvar *rx-queue-lock* (bt:make-lock))
(defvar *tx-q* (jeff:make-queue))
(defvar *rx-q* (jeff:make-queue))

;; Thread stuff.
(defvar *listener-thread* nil)
(defvar *talker-thread* nil)

;; Global for the TCP socket.
(defvar *socket* nil)

(defun create-listener (socket)
  "This is the thread that receives data from JS8Call, processes it,
and queues it up."
  (let ((line nil)
	(proc-line nil))
    (loop ;repeat 10 do
	  (usocket:wait-for-input socket)
	  (setf line (read-line (usocket:socket-stream socket)))
	  (setf proc-line (jsown:parse line))
	  (setf (jsown:val proc-line :raw) line)
	  (setf (jsown:val proc-line :timestamp) (timestamp-to-unix (now)))
	  (bt:with-lock-held (*rx-queue-lock*)
	    (jeff:enqueue (thing-to-obj proc-line) *rx-q*))
	  (bt:with-lock-held (*log-lock*)
	    (setf *log-json* (cons (jsown:to-json proc-line) *log-json*)))
	  (when *verbose*
	    (format t "~%~A~%~%" line)))))

(defun create-talker (socket)
  "This thread pulls data out of the transmit queue and sends it to
JS8Call."
  (loop 
   (unless (jeff:queue-empty-p *tx-q*)
     (bt:with-lock-held (*tx-queue-lock*)
       (format (usocket:socket-stream socket) "~A~C~C"
	       (jeff:dequeue *tx-q*)
	       #\return #\linefeed)
       (force-output (usocket:socket-stream socket))))
   (sleep 0.5)))

(defun start-server (&optional (host "localhost") (port 2442))
  "Start the talked and listener threads."
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
  "Forcibly kill the talker and listener threads."
  (bt:destroy-thread *listener-thread*)
  (bt:destroy-thread *talker-thread*)
  (usocket:socket-close *socket*))

(defun send-message (message)
  "Add a message to the transmit queue."
  (jeff:enqueue message *tx-q*))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End: