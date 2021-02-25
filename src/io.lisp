;;;; io.lisp

(in-package #:js8call)

;; Internal log global and lock.
(defvar *log-lock* (bt:make-lock))
(defvar *log-json* nil)

(defun watch-traffic (&optional (forever nil))
  "Show the received traffic. If an optional t is provided, it will
loop forever."
  (if forever
      (loop
	 (if (jeff:queue-empty-p *rx-q*)
	     (sleep 0.1)
	     (bt:with-lock-held (*rx-queue-lock*)
	       (frame-print (jeff:dequeue *rx-q*)))))
      (mapcar
       (lambda (n)
	 (frame-print n))
       (bt:with-lock-held
	   (*rx-queue-lock*) (jeff:queue-items *rx-q*))))
  t)

(defun write-log (&optional (file-name "log.json"))
  "Write the received messages out to a file as JSON."
  (bt:with-lock-held (*log-lock*)
    (with-open-file (out file-name :direction :output :if-exists :supersede)
      (format out "[~A]~%" (jeff:join (reverse *log-json*) ", ")))))

(defun get-log (&optional (fname "log.json"))
  "Read back the JSON log file."
  (jsown:parse (jeff:file-string fname)))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
