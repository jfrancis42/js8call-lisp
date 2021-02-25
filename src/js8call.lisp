;;;; js8call.lisp

(in-package #:js8call)

(setf *read-default-float-format* 'double-float)

;; Chatty setting.
(defvar *verbose* nil)
(defvar *suppress-activity* nil)
(defvar *suppress-heartbeat* nil)

;; Globals for my station.
(defvar *my-grid* nil)
(defvar *my-call* nil)

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
