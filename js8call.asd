;;;; js8call.asd

(asdf:defsystem #:js8call
  :description "Describe js8call here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:babel
	       #:aviation-formulary
	       #:usocket
	       #:local-time
	       #:bordeaux-threads
	       #:flexi-streams
	       #:cl-ppcre
	       #:jeffutils
	       #:cl-json
	       #:cl-store
	       #:jsown)
  :components ((:file "package")
               (:file "js8call")))
