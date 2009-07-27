;;; -*- mode: lisp; indent-tabs: nil -*-

(in-package :cluck-hunt)

;;;
;;; *data-path* to be set to where
;;; de graphics is stored.
;;;

(defvar *data-path*
  "/home/marcus/src/clbuild/source/cluck-hunt/graphics/")

(defvar *joystick-dev* 0 "Joystick device number to use.")

;;;
;;; Joystick axis setup
;;;
;;; 3 and 4 is what happens to work for me.
;;; For linusians jstest /dev/input/js0 or
;;; somesuch is your friend.
;;;

(defvar *joy-x-axis* 3 "Axis# used for X.")
(defvar *joy-y-axis* 4 "Axis# used for Y.")


