;;;; js8call.lisp

(in-package #:js8call)

(setf *read-default-float-format* 'double-float)

;; Chatty settings.
(defvar *verbose* nil)
(defvar *suppress-id* t)
(defvar *suppress-activity* nil)
(defvar *suppress-heartbeat* nil)
(defvar *print-snr-correctly* nil)

;; Globals for my station.
(defvar *my-grid* nil)
(defvar *my-call* nil)

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
