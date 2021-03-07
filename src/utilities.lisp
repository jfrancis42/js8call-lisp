;;;; utilities.lisp

(in-package #:js8call)

;; Available modem speeds.
(defvar *speed* (list :normal :fast :turbo :invalid :slow))

;; Special JS8Call characters.
(defparameter *eom* "♢")
(defparameter *missing* "…")

(defun band (freq)
  "Given a frequency in khz, return the ham band. Works between 160m
and 70cm."
  (cond
    ((and (>= freq 1800) (<= freq 2000)) :160m)
    ((and (>= freq 3500) (<= freq 4000)) :80m)
    ((and (>= freq 5330) (<= freq 5410)) :60m)
    ((and (>= freq 7000) (<= freq 7300)) :40m)
    ((and (>= freq 10100) (<= freq 10150)) :30m)
    ((and (>= freq 14000) (<= freq 14350)) :20m)
    ((and (>= freq 17068) (<= freq 17168)) :17m)
    ((and (>= freq 21000) (<= freq 21450)) :15m)
    ((and (>= freq 24890) (<= freq 24990)) :12m)
    ((and (>= freq 28000) (<= freq 29700)) :10m)
    ((and (>= freq 50000) (<= freq 54000)) :6m)
    ((and (>= freq 144000) (<= freq 148000)) :2m)
    ((and (>= freq 219000) (<= freq 225000)) :1.25m)
    ((and (>= freq 420000) (<= freq 450000)) :70cm)
    (t nil)))

(defun clean (call)
  "Clean off all the /P cruft from a call."
  (first (split-sequence:split-sequence #\/ call)))

(defun ten-digit-maidenhead (latitude longitude)
  "Horrifically hacky 10-digit Maidenhead implementation (because I
couldn't find one in CL to steal)."
  (let* ((field-letters (load-time-value "ABCDEFGHIJKLMNOPQR" t))
         (subsquare-letters (load-time-value "abcdefghijklmnopqrstuvwx" t))
	 (digits nil)
	 (lon-degrees 360.0)
	 (lat-degrees 180.0)
	 (lon (+ 180.0 longitude))
	 (lat (+ 90.0 latitude))
	 (lon-remainder lon)
	 (lat-remainder lat))
    (flet ((grid-pair (divisions lon-degrees lat-degrees lon lon-remainder lat lat-remainder)
	     (setf lon-degrees (/ lon-degrees divisions))
	     (setf lat-degrees (/ lat-degrees divisions))
	     (setf lon (/ lon-remainder lon-degrees))
	     (setf lat (/ lat-remainder lat-degrees))
	     (list (truncate lon) (truncate lat) lon-degrees lat-degrees
		   lon (nth-value 1 (floor lon-remainder lon-degrees))
		   lat (nth-value 1 (floor lat-remainder lat-degrees)))))
      (destructuring-bind (new-lon new-lat lon-degrees lat-degrees lon lon-remainder lat lat-remainder)
	  (grid-pair 18 lon-degrees lat-degrees lon lon-remainder lat lat-remainder)
	(push new-lon digits)
	(push new-lat digits)
	(destructuring-bind (new-lon new-lat lon-degrees lat-degrees lon lon-remainder lat lat-remainder)
	    (grid-pair 10 lon-degrees lat-degrees lon lon-remainder lat lat-remainder)
	  (push new-lon digits)
	  (push new-lat digits)
	  (destructuring-bind (new-lon new-lat lon-degrees lat-degrees lon lon-remainder lat lat-remainder)
	      (grid-pair 24 lon-degrees lat-degrees lon lon-remainder lat lat-remainder)
	    (push new-lon digits)
	    (push new-lat digits)
	    (destructuring-bind (new-lon new-lat lon-degrees lat-degrees lon lon-remainder lat lat-remainder)
		(grid-pair 10 lon-degrees lat-degrees lon lon-remainder lat lat-remainder)
	      (push new-lon digits)
	      (push new-lat digits)
	      (destructuring-bind (new-lon new-lat lon-degrees lat-degrees lon lon-remainder lat lat-remainder)
		  (grid-pair 24 lon-degrees lat-degrees lon lon-remainder lat lat-remainder)
		(declare (ignore lon-degrees lat-degrees lon lon-remainder lat lat-remainder))
		(push new-lon digits)
		(push new-lat digits))))))
      (format nil "~c~c~d~d~c~c~d~d~c~c"
              (char field-letters (nth 9 digits)) (char field-letters (nth 8 digits))
	      (nth 7 digits) (nth 6 digits)
              (char subsquare-letters (nth 5 digits)) (char subsquare-letters (nth 4 digits))
	      (nth 3 digits) (nth 2 digits)
              (char subsquare-letters (nth 1 digits)) (char subsquare-letters (nth 0 digits))))))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
