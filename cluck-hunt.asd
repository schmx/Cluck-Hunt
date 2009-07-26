;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :cluck-hunt
  :serial t
  ;; add new files to this list:
  :components ((:file "package")
	       (:file "configuration")
	       (:file "cluck-hunt"))
  :depends-on (:lispbuilder-sdl)
;  :depends-on (#+nil :cl-ppcre)
  )
