;;; -*- mode: lisp; indent-tabs: nil -*-

;;; CLUCK HUNT

(in-package :cluck-hunt)

(defun wait-button ()
  (with-events (:wait)
    (:joy-button-down-event ()
     (return-from wait-button))))

(defun event-loop ()
  (with-events (:poll)
    (:quit-event () t)
    (:video-expose-event (sdl:update-display))
    (:KEY-DOWN-EVENT (:key key)
	 (when (key= key :sdl-key-escape)
	   (sdl:push-quit-event)))
    (:JOY-BUTTON-DOWN-EVENT (:WHICH WHICH :BUTTON BUTTON :STATE STATE)
	(declare (ignorable which button state))
	(format t "pew pew!~%"))
    (:JOY-AXIS-MOTION-EVENT (:WHICH WHICH :AXIS AXIS :VALUE VALUE)
			    (format t "which: ~d  axis:~d   value:~d~%" which axis value))
    (:idle ()
	   ;; quack quack!!
	   )))

(defstruct screen
  title
  text)

(defvar &start-screen&
  (make-screen 
   :title "CLUCK HUNT 2009 EDITION"
   :text '("Hello!"
	   ""
	   "All these darned chickens are running around and"
	   "driving me crazy! YEEEEHAW!"
	   "Help me get rid of them by pointing your cross hairs at them"
	   "and tap the PEW!PEW!PEW! button."
	   ""
	   ""
	   "Help me Obi-Gun Cluckobi! You're my only hope..."
	   ""
	   ""
	   ""
	   ""
	   ""
	   "                        Tap PEW!PEW!PEW! to start.")))

(defun display (screen)
  (with-color (col *red*)
    (draw-string-solid-* (screen-title &start-screen&) 0 0))
  (with-color (col *white*)
    (loop for line in (screen-text screen)
	  for y from 25 by 25
	  doing (draw-string-solid-* line
		     0 y)))
  (update-display))


(defun init-video ()
  (window 640 480 :bpp 16
	  :title-caption "Cluck hunt!")
  (setf *default-font*
	(sdl:initialise-default-font sdl:*font-10x20*)))

(defun cluck-hunt ()
  (format t "Hello World from Cluck Hunt.~%")
  (with-init (sdl-init-video sdl-init-joystick)
    (let ((num-stick (num-joysticks)))
      (unless (< num-stick 1)
	;; we have joystick.
	(sdl-cffi::sdl-joystick-open *joystick-dev*) ; TODO add error check
	(init-video)
	(display &start-screen&)
	(wait-button)
	(update-display)
	
	(event-loop))
      ))
    
)

