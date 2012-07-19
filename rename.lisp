;;;; Picture renamer

;;(in-package :cl-picrename)

(ql:quickload "cl-opengl")   ; load OpenGL bindings
(ql:quickload "cl-glu")      ; load GLU bindings
(ql:quickload "cl-glut")     ; load GLUT bindings
(ql:quickload "cl-devil")
(ql:quickload "bordeaux-threads")
(ql:quickload "closer-mop")
(ql:quickload "alexandria")
;;(asdf:load-system "fsm")
(load "../fsm/fsm.lisp")
(asdf:load-system "utilities")
(asdf:load-system :cl-fad)
(ql:quickload "lisp-magick")

(defvar *config-file* ".renamerrc")
(defvar *list-of-files* '())
(defvar *inputmap* (make-hash-table :test #'eq))
(defvar *name-pattern* "<names> <description> <#> <date>")
(defvar *name-in-progress* "")
(defvar <names> nil)
(defvar <description> (list))
(defvar *description-in-progress* "")
(defvar <date> nil)
(defvar *current-word* "")
(defvar *the-state* "")
(defparameter *prompt* "prompt>")
(defparameter out *standard-output*)

(defvar prompt-lock (bordeaux-threads:make-lock))

(defclass word ()
  ((the-char :accessor the-char :initarg :the-char :initform (error "the-char needs to be provided"))
   (word :accessor word :initarg :word :initform (error "Word needs to be provided"))))

(defclass description (word)
  ())

(defun rename (file1 file2)
  (format out "Check that file 1 exists and 2 doesn't~%Rename ~a to ~a~%" file1 file2))

(defun toggle-name (name)
  (if (find name <names>)
      (delete name <names>)
      (push name <names>)))

(defun get-list-of-files ()
  (list "/home/nick/lisp/cl-picrename/examples/2012-02-01 13.17.41.jpg"
	"/home/nick/lisp/cl-picrename/examples/2012-02-01 13.18.09.jpg"
	"/home/nick/lisp/cl-picrename/examples/2012-02-03 15.33.56.jpg"
	"/home/nick/lisp/cl-picrename/examples/2012-02-03 15.34.08.jpg"))

(defun words-equalp (c1 c2)
  (eq (the-char c1) (the-char c2)))

(defun compile-name ()
  (format nil *title-format*
	  (mapcar (lambda (name)
		    (word name))
		  <names>)
	  (car <description>)))

(defun revert-description (num)
  (let ((desc (nth num <description>)))
    (cond (desc (push desc <description>))
	  (t (push (car (last <description>)) <description>)))))

(defun make-description ()
  (let* ((desc (prompt-read "Description or #0-#9 for previous descriptions"))
	(num (handler-case (parse-integer desc)
	       (error () nil))))
    (cond ((and num (max (min num 9) 0)) (revert-description num))
	  (num (make-description))
	  (t (push desc <description>)))
    (delete-duplicates <description> :from-end t)))

(defun make-name (input-char)
  (let* ((name (prompt-read "Name"))
	 (name-object (make-instance 'word :word name :the-char input-char)))
    (setf (gethash input-char *inputmap*) name-object)
    (toggle-name name-object)))

(fsm:deffsm input-fsm ()
  ((the-key :accessor the-key)))

(fsm:defstate input-fsm :initial (fsm c)
  (let ((in-mapped (gethash c *inputmap*)))
    (cond
      (in-mapped (progn (toggle-name in-mapped)
			:named))
      (t (progn (setf (the-key fsm) c)
		(format out "Character ~A, beginning naming~%" c)
		(setf *prompt* (format nil "Name (~A): " c))
		:naming)))))

(fsm:defstate input-fsm :named (fsm c)
  (case c
    (#\Newline ; Finish the name of the file
     (progn (rename (pop *list-of-files*) (compile-name))
	    (setf <names> nil)
	    :initial))
    (otherwise
     (let ((in-mapped (gethash c *inputmap*)))
       (cond
	 (in-mapped (progn (toggle-name in-mapped)
			   (format out "Character ~A in map~%" c)
			   ;; Have to check if we've turned off a name
			   (if (or <names>
				   <description>)
			       :named
			       :initial)))
	 (t (progn (setf (the-key fsm) c)
		   (format out "Character ~A, beginning naming~%" c)
		   (setf *prompt* (format nil "Name (~A): " c))
		   :naming)))))))
    
(fsm:defstate input-fsm :naming (fsm c)
  (case c
    (#\Newline (progn (rename (pop *list-of-files*) (compile-name))
		      (format out "~%Character ~A set to ~A" (the-key fsm) *name-in-progress*)
		      (setf <names> nil)))
    (t (progn (concatenate 'string *name-in-progress* (list c))
	      (format out "~A" c)
	      (bordeaux-threads:acquire-lock prompt-lock)
	      (concatenate 'string *prompt* (list c))
	      (bordeaux-threads:release-lock prompt-lock)
	      :naming))))

(defparameter *the-fsm* (make-instance 'input-fsm))

;; (format t "Commands~%l - Load new files~%; - Make a new description~%<NewLine> - Execute renaming of file")
;; (format t "To add names, initially type the letter which will provide the hotkey for the person you want to add.~%When prompted, type the name and hit enter.  Subsequent use of that letter hotkey will result in the toggling of that person's name.~%")

(defparameter out *standard-output*)
(defparameter *the-window* nil)

(defclass my-window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p)
   (texture-id :initform nil :accessor texture-id))
  (:default-initargs :width 400 :height 300
                     :title "Renamer"
                     :x 100 :y 100
                     :mode '(:double :rgb :depth)
                     :fullscreen nil
                     :tick-interval (round 1000 60)))  ; milliseconds per tick

(defmethod glut:tick ((win my-window))
  (glut:post-redisplay))        ; tell GLUT to redraw

(defmethod glut:display-window :before ((win my-window))
  (gl:shade-model :smooth)        ; enables smooth shading
  (gl:clear-color 0 0 0 0)        ; background will be black
  (gl:clear-depth 1)              ; clear buffer to maximum depth
  (gl:enable :depth-test)         ; enable depth testing
  (gl:depth-func :lequal)         ; okay to write pixel if its depth
                                  ; is less-than-or-equal to the
                                  ; depth currently written
                                  ; really nice perspective correction
  (gl:hint :perspective-correction-hint :nicest)

  (when (fullscreen-p win)        ; check to see if fullscreen needed
    (glut:full-screen))           ; if so, then tell GLUT

  (unless (texture-id win)     ; load texture if needed
    (setf (texture-id win)
          (il:with-init
	    (ilut:renderer :opengl)
	    (ilut:gl-load-image "/home/nick/2012-04-07-09.19.53.jpg"))))
;;	    (ilut:gl-load-image "/home/nick/2012-04-07-09.19.53.jpg"))))
;;	    (ilut:gl-load-image "/home/nick/lisp/cl-picrename/examples/pic1.jpg"))))
  (when (texture-id win)       ; enable texturing if we have one
    (gl:enable :texture-2d)))

(defun glut-print (x y font text r g b a)
  "http://www.gamedeception.net/threads/1876-Printing-Text-with-glut
Print with glut to an x, y with a glut:font"
  (let ((blending (gl:enabledp :blend)))
    (gl:enable :blend)
    (gl:color r g b a)
    (gl:raster-pos x y)
    (map nil
	 #'(lambda (c)
	     (glut:bitmap-character font
				    (char-int c)))
	 text)
    (unless blending
      (gl:disable :blend))))

(defmethod glut:display ((win my-window))
  ;; clear the color buffer and depth buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:color 1 1 1 1)
  (gl:load-identity)              ; reset the modelview matrix

  (when (texture-id win)          ; bind the texture if we have it
    (gl:bind-texture :texture-2d (texture-id win)))
  
  (utilities:condlet (((fullscreen-p win) (height (glut:get :screen-height)) (width (glut:get :screen-width)))
	    (t (height (glut:height win)) (width (glut:width win))))
    (gl:with-primitives :quads
      ;; front face
      (gl:tex-coord 0.0 1.0) (gl:vertex 0.0 height 0.0)
      (gl:tex-coord 1.0 1.0) (gl:vertex width  height  0.0)
      (gl:tex-coord 1.0 0.0) (gl:vertex width  0.0 0.0)
      (gl:tex-coord 0.0 0.0) (gl:vertex 0.0 0.0 0.0)))

  ;; Text overlay
  (bordeaux-threads:acquire-lock prompt-lock)
  (glut-print 10 10 glut:+bitmap-9-by-15+
	      *prompt* 0 0 0 1)
  (bordeaux-threads:release-lock prompt-lock)
  ;; swap the buffer onto the screen  
  (glut:swap-buffers))

(defmethod glut:reshape ((win my-window) width height)
  (gl:viewport 0 0 width height)  ; reset the current viewport
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix

  ;; set perspective based on window aspect ratio
;;  (glu:perspective 45 (/ width (max height 1)) 1/10 100)
  (glu:ortho-2d 0 width 0 height)

  ;; (format out "Win: Height: ~A, Width: ~A~%" (glut:height win) (glut:width win))
  ;; (format out "New: Height: ~A, Width: ~A~%" height width)

  (setf (glut:height win) height
  	(glut:width win) width)

  (gl:matrix-mode :modelview)     ; select the modelview matrix
  (gl:load-identity))              ; reset the matrix

(defmethod glut:keyboard ((win my-window) key xx yy)
  (declare (ignore xx yy))
  ;; Check for special keys
  (multiple-value-bind (shift ctrl alt) (glut:get-modifier-values)
    (declare (ignore shift ctrl))
    (if alt
	(case key
	  ((#\f #\F)
	   ;; save whether we're in fullscreen
	   (let ((full (fullscreen-p win)))
	     (glut:destroy-current-window)       ; close the current window
	     (glut:display-window   ; open a new window with fullscreen toggled
	      (make-instance 'my-window
			     :fullscreen (not full)))))
	  ((#\l #\L) (progn (setf *list-of-files* (get-list-of-files))
;;			    (format out "Getting list of files~%~{~A~%~}" *list-of-files*)
			    (setf (texture-id win)
				  (il:with-init
				    (ilut:renderer :opengl)
				    (ilut:gl-load-image (car *list-of-files*)))))))
	  
	;; No alt modifier, do normal stuff
	(case key
	  ((#\Escape) (glut:destroy-current-window))
	  (otherwise
	   (funcall *the-fsm* key))))))
	  ;; ((#\m #\M) (multiple-value-bind (shift ctrl alt) (glut:get-modifier-values)
	  ;; 		 (format out "SHIFT ~A~%CTRL  ~A~%alt   ~A~%" shift ctrl alt)))
	  ;; (#\Newline (progn (rename (pop *list-of-files*) (compile-name))
	  ;; 		    (setf <names> nil)))
	  ;; (#\; (make-description))
	  ;; (otherwise
	  ;;  (let ((in-mapped (gethash key *inputmap*)))
	  ;;    (cond
	  ;;      (in-mapped (toggle-name in-mapped))
	  ;;      (t (make-name key)))))))))

;; (defmethod glut:keyboard-up ((win my-window) key xx yy)
;;   (declare (ignore xx yy))
;;   (case key
;;     ((#\q #\Q #\Escape) t)))

(defun run-it ()
  (setf *the-window* (make-instance 'my-window))
  (glut:display-window *the-window*))
