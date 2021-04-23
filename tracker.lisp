;;;; tracker.lisp

(setf *read-default-float-format* 'double-float)

(ql:quickload :js8call)
(ql:quickload :gpsd)
(in-package #:js8call)

(defun start-gpsd ()
  (gpsd:start-gpsd "127.0.0.1" 2947))

(defun tracker-loop (interval)
  (let ((now (- (local-time:timestamp-to-unix (local-time:now)) interval -30))
	(here nil))
    (loop
      (setf here (gpsd:get-current-location))
      (gpsd:pp here)
      (format t "~%~A~%~%" (ten-digit-maidenhead (gpsd:point-lat here)
					       (gpsd:point-lon here)))
      (when (>= (local-time:timestamp-to-unix (local-time:now)) (+ interval now))
	(setf now (local-time:timestamp-to-unix (local-time:now)))
	(set-grid-square
	 (ten-digit-maidenhead (gpsd:point-lat here)
			       (gpsd:point-lon here)))
	(sleep 1)
	(send-grid-square-to-aprs))
      (sleep 30))))

(defun track (&optional (interval 600))
  (start-gpsd)
  (sleep 5)
  (tracker-loop interval))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
