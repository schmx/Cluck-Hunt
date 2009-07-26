;;; -*- mode: lisp; indent-tabs: nil -*-

;;; CLUCK HUNT
;;;
;;; This is a really fun game to play.
;;; You need a joystick!
;;;
;;; TODO: Draw level background
;;; TODO: Make the crosshairs move
;;; TODO: Make a pause function + pause-screen
;;;

(in-package :cluck-hunt)

(defvar *joystick-device* nil)

(defvar *crosshairs-x* 320)
(defvar *crosshairs-y* 240)
(defvar *crosshair-gfx* nil)

(defun wait-button ()
  (with-events (:wait)
    (:joy-button-down-event ()
     (return-from wait-button))))

(defun in-range (n lower upper)
  (and (>= n lower)
       (<= n upper)))

(defun move-crosshairs (axis value)
  (let ((pixels (round value 10000)))
    ;; Move the crosshairs if possible.
    (macrolet ((update (var max min)
		 `(let ((new (+ ,var pixels)))
		    (when (in-range new ,max ,min))
		    (setf ,var new))))
      (cond ((= axis *joy-x-axis*)
	     (update *crosshairs-x* 20 620))
	    ((= axis *joy-y-axis*)
	     (update *crosshairs-y* 20 300))))))

(defun get-joy-axes ()
  (let ((x (sdl-cffi::sdl-joystick-get-axis *joystick-device* 3))
	(y (sdl-cffi::sdl-joystick-get-axis *joystick-device* 4)))
    (format t "x:~d y:~d~%~%" x y)))

(defun event-loop ()
  (setf (sdl:frame-rate) 30)
  (with-events (:poll)
    (:quit-event () t)
    (:video-expose-event (sdl:update-display))
    (:KEY-DOWN-EVENT (:key key)
	 (when (key= key :sdl-key-escape)
	   (sdl:push-quit-event)))
    (:JOY-BUTTON-DOWN-EVENT (:WHICH WHICH :BUTTON BUTTON :STATE STATE)
	(declare (ignorable which button state))
	(format t "pew pew!~%"))
    (:idle ()
       (get-joy-axes)   
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
    ;; Display title.
    (draw-string-solid-* (screen-title &start-screen&) 0 0))
  (with-color (col *white*)
    ;; Display text.
    (loop for line in (screen-text screen)
	  for y from 25 by 25
	  doing (draw-string-solid-* line
		     0 y)))
  (update-display))

(defun init-video ()
  (window 640 480 :bpp 16
	  :title-caption "Cluck hunt!"
	  :flags '(sdl-doublebuf #|sdl-fullscreen|#))
  (setf *default-font*
	(sdl:initialise-default-font sdl:*font-10x20*)))

(defun load-data ()
  (setf *crosshair-gfx*
	(load-image "/home/marcus/src/clbuild/source/cluck-hunt/graphics/crosshairs.gif"))
  (inspect *crosshair-gfx*))

(defun cluck-hunt ()
  (format t "Hello World from Cluck Hunt. Hope you have a working joystick!~%")
  (with-init (sdl-init-video sdl-init-joystick)
    (let ((num-stick (num-joysticks)))
      (unless (< num-stick 1)
	;; We have joystick.
	(setf *joystick-device*
	      (sdl-cffi::sdl-joystick-open *joystick-dev*)) ; TODO Add error check.
	(load-data)
	(init-video)
	(display &start-screen&)
	(wait-button)
	(update-display)
	(event-loop)))))


