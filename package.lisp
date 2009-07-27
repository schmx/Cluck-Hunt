;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :cluck-hunt
  (:use :lispbuilder-sdl :lispbuilder-sdl-image :cl)
  (:shadowing-import-from :lispbuilder-sdl-image :load-image
			  :image-p :image-type-of)
  (:export #:cluck-hunt))
