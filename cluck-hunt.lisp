;;; -*- mode: lisp; indent-tabs: nil -*-

;;; CLUCK HUNT
;;;
;;; This is a really fun game to play.
;;; You need a joystick!
;;;
;;; TODO: Add some chickens.
;;; TODO: Make a pause function + pause-screen.
;;;

(in-package :cluck-hunt)

(defvar *joystick-device* nil)
(defvar *joystick-available* nil)
(defstruct entity 
  x
  y
  gfx)

(defvar &crosshairs&
  (make-entity :x 320 :y 240 :gfx nil))

(defvar *background-gfx* nil)

(defun wait-button ()
  (with-events (:wait)
    (:joy-button-down-event ()
     (return-from wait-button))
    (:key-down-event ()
     (return-from wait-button))))

(defun in-range (n lower upper)
  (and (>= n lower)
       (<= n upper)))

(defun move (entity x y)
  ;; Move ENTITY in X/Y if allowed.
  (let ((x-pixels (round x 10000))
	(y-pixels (round y 10000)))
    (macrolet ((update (loc pixels min max)
		 `(let ((new (+ ,loc ,pixels)))
		    (when (in-range new ,min ,max)
		      (setf ,loc new)))))
      (update (entity-x entity) x-pixels 20 620)
      (update (entity-y entity) y-pixels 20 4600))))

(defun get-joy-axes ()
  (let ((x (sdl-cffi::sdl-joystick-get-axis *joystick-device* 3))
	(y (sdl-cffi::sdl-joystick-get-axis *joystick-device* 4)))
    (values x y)))

(defun draw (entity)
  (draw-surface-at-*
   (entity-gfx entity)
   (entity-x entity)
   (entity-y entity)))

(defun get-keyboard ()
  ;; TODO This function is in serious need of
  ;;      factoring.
  (let ((x (+ (if (get-key-state :sdl-key-left)
		  -32767 0)
	      (if (get-key-state :sdl-key-right)
		  32767 0)))
	(y (+ (if (get-key-state :sdl-key-up)
		  -32767 0)
	      (if (get-key-state :sdl-key-down)
		  32767 0))))
    (values x y)))

(defun get-player-input ()
  (if *joystick-available*
      (get-joy-axes)
      (get-keyboard)))

(defun update-player ()
  (multiple-value-bind (x y)
      (get-player-input)
    (move &crosshairs& x y)))

(defun event-loop ()
  (setf (sdl:frame-rate) 30)
  (with-events (:poll)
    (:quit-event () t)
    (:video-expose-event (sdl:update-display))
    (:KEY-DOWN-EVENT (:key key)
	 (when (key= key :sdl-key-escape)
	   (push-quit-event)))
    (:JOY-BUTTON-DOWN-EVENT (:WHICH WHICH :BUTTON BUTTON :STATE STATE)
	(declare (ignorable which button state))
	(format t "pew pew!~%"))
    (:idle ()
        (draw-surface *background-gfx*)
        (draw &crosshairs&)
        (update-display)
	(update-player)
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
	  doing (draw-string-solid-* line 0 y)))
  (update-display))

(defun init-video ()
  (window 640 480 :bpp 16
	  :title-caption "Cluck hunt!"
	  :flags '(sdl-doublebuf #|sdl-fullscreen|#))
  (setf *default-font*
 	(sdl:initialise-default-font sdl:*font-10x20*)))

(defun load-data ()
  (setf (entity-gfx &crosshairs&)
	(load-image "/home/marcus/src/clbuild/source/cluck-hunt/graphics/crosshairs.png"))
  (enable-alpha t :surface (entity-gfx &crosshairs&))
  (setf *background-gfx*
	(load-image "/home/marcus/src/clbuild/source/cluck-hunt/graphics/background.gif")))

(defun cluck-hunt ()
  (format t "Hello World from Cluck Hunt. Hope you have a working joystick!~%")
  (with-init (sdl-init-video sdl-init-joystick)
    (enable-key-repeat nil nil)
    (unless (< (num-joysticks) 1)
      ;; We have joystick.
      (setf *joystick-device*
	    (sdl-cffi::sdl-joystick-open *joystick-dev*))
      (setf *joystick-available* t))
    (load-data)
    (init-video)
    (display &start-screen&)
    (wait-button)
    (event-loop)))
