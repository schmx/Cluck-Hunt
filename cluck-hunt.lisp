;;; -*- mode: lisp; indent-tabs: nil -*-

;;; CLUCK HUNT
;;;
;;; This is a really fun game to play.
;;; This be an experiment in lispbuilder-sdl joystick handling.
;;;   Also you can play with the keyboard.
;;;
;;; TODO: Add killing of poultry.
;;; TODO: Add ammo display.
;;; TODO: Add more frames to the birds. Needs rewrite of
;;;       BIRDS-FLAPS-ITS-WINGS-OR-FALLS-DOWN.
;;; TODO: Make a pause function + pause-screen.
;;; TODO: Fix the stupid keyboard handling!
;;;       fire + two direction keys seem to malfunction :)
;;;

(in-package :cluck-hunt)

(define-condition game-over (condition) ())

(defvar *joystick-device* nil)
(defvar *joystick-available* nil)

(defstruct entity 
  x
  y
  gfx)

(defvar &crosshairs&
  (make-entity :x 320 :y 240 :gfx nil))
(defvar *ammo* 0)

(defvar &bird& nil)
(defvar *bird-speed* nil "Average air speed velocity.")
(defvar *bird-destination-x* nil)
(defvar *bird-destination-y* nil)
(defvar *bird-frame* nil)
(defvar *bird-gfxs* nil)

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
      (update (entity-y entity) y-pixels 20 460))))

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

(defun random-x ()
  (+ 20 (random 600)))
(defun random-y ()
  (+ 20 (random 380)))
(defun fuzzy= (n1 n2)
  (in-range n1 (- n2 10) (+ n2 10)))
(defun fuzzy-at-xy? (entity x y)
  (and
   (fuzzy= (entity-x entity) x)
   (fuzzy= (entity-y entity) y)))

(defun hatch-an-egg ()
  "Creates a new bird."
  (make-entity :x (random-x)
	       :y (random-y)
	       :gfx (caar *bird-gfxs*)))
(defun bird-has-wanderlust! ()
  "Sets up a new destination for bird."
  (setf *bird-destination-x* (random-x))
  (setf *bird-destination-y* (random-y)))
(defun african-or-european-swallow? ()
  "Sets speed of bird."
  (setf *bird-speed* (+ 10000 (random 30000))))
(defun bird-flaps-its-wings-or-falls-down! (x)
  "Animate ze bird by settin gfx."
  ;; TODO This is the most beautiful piece of
  ;;      code ever. TODO is to never touch it.
  (setf (entity-gfx &bird&)
	(if (> 0 x)
	    (if *bird-frame*
		(progn (setf *bird-frame* nil)
		       (cadar *bird-gfxs*))
		(progn (setf *bird-frame* t)
		       (caar *bird-gfxs*)))
	    (if *bird-frame*
		(progn (setf *bird-frame* nil)
		       (cadadr *bird-gfxs*))
		(progn (setf *bird-frame* t)
		       (caadr *bird-gfxs*))))))
(defun bird-is-going-somewhere! ()
  "Move &bird& towards destination, and animates.."
  ;; TODO Factor it up.
  (let ((x (if (< (entity-x &bird&) *bird-destination-x*)
	       *bird-speed*
	       (- *bird-speed*)))
	(y (if (< (entity-y &bird&) *bird-destination-y*)
	       *bird-speed*
	       (- *bird-speed*))))
    (move &bird& x y)
    (bird-flaps-its-wings-or-falls-down! x)))

(defun update-bird ()
  (when (null &bird&)
    ;; Thar be no bird!
    (setf &bird& (hatch-an-egg))
    (bird-has-wanderlust!)
    (african-or-european-swallow?))
  (when (fuzzy-at-xy? &bird& 
	      *bird-destination-x*
	      *bird-destination-y*)
      (bird-has-wanderlust!))
  (bird-is-going-somewhere!))

(defun bird-is-close-enough? ()
  ;; offset into middle of bird, compare
  ;; to crosshairs x/y
  nil
  )
(defun kill-that-bird ()
  (setf &bird& nil))
(defun reload-gun ()
  (setf *ammo* 3))
(defun bangbangbang ()
  (unless (<= *ammo* 0)
    ;; TODO play bang sound
    (decf *ammo*)
    (when (bird-is-close-enough?)
      (kill-that-bird)
      (reload-gun)))
  (when (<= *ammo* 0)
    (signal 'game-over)))

(defun event-loop ()
  (setf (sdl:frame-rate) 30)
  (with-events (:poll)
    (:quit-event () t)
    (:video-expose-event (sdl:update-display))
    (:KEY-DOWN-EVENT (:key key)
	 ;; TODO: move :sdl-key-space into :idle
	 (when (key= key :sdl-key-escape)
	   (push-quit-event))
	 (when (key= key :sdl-key-space)
	   (bangbangbang)))
    (:joy-button-down-event ()
	(bangbangbang))
    (:idle ()
        (draw-surface *background-gfx*)
	(update-bird)
	(draw &bird&)
        (draw &crosshairs&)
        (update-display)
	(update-player))))

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

(defun image (name)
  (merge-pathnames *data-path* name))

(defun load-bird-gfx ()
  ;; TODO: Factor this. (:
  (let ((a
	 (load-image (image "bird-left1.png")))
	(b
	 (load-image (image "bird-left2.png")))
	(c
	 (load-image (image "bird-right1.png")))
	(d
	 (load-image (image "bird-right2.png"))))
    (enable-alpha t :surface a)
    (enable-alpha t :surface b)
    (enable-alpha t :surface c)
    (enable-alpha t :surface d)
    (setf *bird-gfxs* (list (list a b)
			    (list c d)))))

(defun load-data ()
  (setf (entity-gfx &crosshairs&)
	(load-image (image "crosshairs.png")))
  (enable-alpha t :surface (entity-gfx &crosshairs&))
  (setf *background-gfx*
	(load-image (image "background.gif")))
  (load-bird-gfx))

(defun cluck-hunt ()
  (format t "Hello World from Cluck Hunt. Hope you have a working joystick!~%")
  (with-init (sdl-init-video sdl-init-joystick)
    (handler-bind ((game-over #'(lambda (x)
				  (declare (ignore x))
				  (format t "Out of ammo! Game over.~%")
				  (return-from nil))))
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
      (setf *ammo* 3)
      (event-loop))))
