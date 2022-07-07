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
	   :*verbose*
	   :*suppress-id*
	   :*suppress-activity*
	   :*suppress-heartbeat*
	   :*suppress-cq*
	   :*print-snr-correctly*
	   :kill-server
	   :start-server
	   :restart-server
	   :pp
	   :watch-traffic
	   :write-log
	   :get-log
	   :loud
	   :quiet
	   :get-grid
	   :get-distance
	   :get-bearing
	   :get-inbox-messages
	   :get-rig-freq
	   :set-rig-freq
	   :get-callsign
	   :get-grid-square
	   :get-info
	   :set-grid-square
	   :set-info
	   :get-call-activity
	   :get-call-selected
	   :get-band-activity
	   :get-rx-text
	   :get-tx-text
	   :set-tx-text
	   :send-text
	   :get-speed
	   :set-speed
	   :window-raise
	   :snr?
	   :grid?
	   :info?
	   :status?
	   :hearing?
	   :agn?
	   :qsl?
	   :send-grid-square-to-aprs
	   :send-heartbeat
	   :query-messages
	   :query-hearing-call
	   :send-sms
	   :send-email
	   :send-pota
	   :send-directed-message
	   ))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
