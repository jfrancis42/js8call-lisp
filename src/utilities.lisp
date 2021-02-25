;;;; utilities.lisp

(in-package #:js8call)

;; Available modem speeds.
(defvar *speed* (list :normal :fast :turbo :invalid :slow))

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
