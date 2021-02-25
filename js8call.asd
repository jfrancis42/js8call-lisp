;;;; js8call.asd

(asdf:defsystem #:js8call
  :description "Package to talk to the JS8Call TCP API."
  :author "Jeff Francis <jeff@gritch.org>"
  :license  "MIT, see file LICENSE"
  :version "0.0.2"
  :serial t
  :depends-on (#:babel
	       #:usocket
	       #:local-time
	       #:bordeaux-threads
	       #:flexi-streams
	       #:cl-ppcre
	       #:jeffutils
	       #:jsown)
  :components ((:file "src/package")
               (:file "src/js8call")
               (:file "src/utilities")
               (:file "src/api")
               (:file "src/io")
               (:file "src/frames")
               (:file "src/queries") ))
