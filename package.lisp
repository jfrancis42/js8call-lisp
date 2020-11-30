;;;; package.lisp

(defpackage #:js8call
  (:use #:cl)
  (:import-from :jeffutils
		:cdr-assoc
		:clean-string
		:remove-duplicate-strings
		:english-join
		:join
		:string-or-nil)
  (:import-from :alexandria
		:hash-table-keys
		:hash-table-values)
  (:import-from :split-sequence
		:split-sequence)
  (:import-from :local-time
		:now
		:timestamp-to-unix
		:unix-to-timestamp)
  (:export :*heard*
	   :*grids*
	   ))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
