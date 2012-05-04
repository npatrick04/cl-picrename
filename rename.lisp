;;;; Picture renamer

;;(in-package :cl-picrename)

(ql:quickload "cl-opengl")   ; load OpenGL bindings
(ql:quickload "cl-glu")      ; load GLU bindings
(ql:quickload "cl-glut")     ; load GLUT bindings
(ql:quickload "cl-devil")
(ql:quickload "bordeaux-threads")
(asdf:load-system "utilities")

(defvar *config-file* ".renamerrc")
(defvar *list-of-files* '())
(defvar *inputmap* (make-hash-table :test #'eq))
(defvar *name-pattern* "<names> <description> <#> <date>")
(defvar *title-format* "~{~a~#[~; and ~:;, ~]~} ~a num date~%")
(defvar <names> nil)
(defvar <description> (list))
(defvar <date> nil)
(defvar *current-word* "")
(defvar *state* 'idle) ;;; idle, new, nextfile, modified, set-name, set-description, enter-description, enter-name

(defclass word ()
  ((the-char :accessor the-char :initarg :the-char :initform (error "the-char needs to be provided"))
   (word :accessor word :initarg :word :initform (error "Word needs to be provided"))))

(defclass description (word)
  ())

(defun rename (file1 file2)
  (format t "Check that file 1 exists and 2 doesn't~%Rename ~a to ~a~%" file1 file2))

(defun toggle-name (name)
  (if (find name <names>)
      (delete name <names>)
      (push name <names>)))
;;(trace find remove)

(defun get-list-of-files ()
  (list "/home/nick/lisp-code/cl-picrename/examples/pic1.jpg"
	"/home/nick/lisp-code/cl-picrename/examples/pic2.jpg"
	"/home/nick/lisp-code/cl-picrename/examples/pic3.jpg"))

(defun destroy-window ()
  (format t "DESTROY THE WINDOW!!!~%"))

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

(defun prompt-read (prompt)
  (fresh-line)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (force-output *standard-input*)
  (read-line *query-io*))

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

(defun input-handler (input-char)
  (let ((in-mapped (gethash input-char *inputmap*)))
    (case input-char
      (#\l (setf *list-of-files* (get-list-of-files)))
      (#\Newline (progn (rename (pop *list-of-files*) (compile-name))
			(setf <names> nil)))
      (#\; (make-description))
      (otherwise
       (cond
	 (in-mapped (toggle-name in-mapped))
	 (t (make-name input-char)))))))
;;(defun input-handler (input-char)
;;  (let ((in-mapped (gethash input-char *inputmap*))
;;	(num (search (list input-char) "0123456789")))
;;    (cond ((eq input-char #\.) (destroy-window))
;;	  ((eq input-char #\Newline) (rename-file (compile-name)))
;;;;;;	  ((and (eq *state* 'set-description) (eq input-char #\;)) (push "" <description>))
;;	  ((eq input-char #\;) (setf *state* 'set-description))
;;	  ((and (eq *state* 'set-description) num) (revert-description num))
;;	  ((eq *state* 'set-description) (make-description))
;;	  (in-mapped (toggle-name in-mapped))
;;	  (t (make-name input-char)))))

;; (format t "Commands~%l - Load new files~%; - Make a new description~%<NewLine> - Execute renaming of file")
;; (format t "To add names, initially type the letter which will provide the hotkey for the person you want to add.~%When prompted, type the name and hit enter.  Subsequent use of that letter hotkey will result in the toggling of that person's name.~%")
;; (princ "Press Escape to exit the program")
;; (format t "Input: ")
;; (loop for ch = (read-char)
;;       until (eq ch #\Escape)
;;       do (input-handler ch)
;;          (fresh-line)
;;          (format t "List of files to rename~%*current*~{~A~%~}~a~%Input: " *list-of-files* (compile-name)))

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
;;	    (ilut:gl-load-image "/home/nick/lisp/cl-picrename/examples/pic1.jpg"))))
  (when (texture-id win)       ; enable texturing if we have one
    (gl:enable :texture-2d)))

(defmethod glut:display ((win my-window))
  ;; clear the color buffer and depth buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)
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
      (gl:tex-coord 0.0 0.0) (gl:vertex 0.0 0.0 0.0))

    ;; Text overlay
    ;; (gl:matrix-mode :projection)
    ;; (gl:push-matrix)
    ;; (gl:load-identity)
    ;; (glu:ortho-2d 0 width 0 height)
    
    ;; (gl:matrix-mode :modelview)
    ;; (gl:push-matrix)
    ;; (gl:load-identity)

    (gl:color 0 1 0)
    (gl:raster-pos 10 10)
    (glut:bitmap-character glut:+bitmap-9-by-15+
			   65))
    ;; (let ((s "Respect mah authoritah!"))
    ;;   (map 'string #'(lambda (c)
    ;; 		       (when (and c
    ;; 				  (char-int c))
    ;; 			 (glut:bitmap-character glut:+bitmap-9-by-15+
    ;; 						(char-int c)))
    ;; 		       nil)
    ;; 	   s)))

  ;; (gl:matrix-mode :modelview)
  ;; (gl:pop-matrix)

  ;; (gl:matrix-mode :projection)
  ;; (gl:pop-matrix)

  (glut:swap-buffers))             ; swap the buffer onto the screen

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
  (case key
    ((#\q #\Q #\Escape) (glut:destroy-current-window))
    ((#\m #\M) (multiple-value-bind (shift ctrl alt) (glut:get-modifier-values)
		 (format out "SHIFT ~A~%CTRL  ~A~%alt   ~A~%" shift ctrl alt)))
    ((#\f #\F)                  ; when we get an 'f'
                                ; save whether we're in fullscreen
       (let ((full (fullscreen-p win)))
         (glut:destroy-current-window)       ; close the current window
         (glut:display-window   ; open a new window with fullscreen toggled
             (make-instance 'my-window
                            :fullscreen (not full)))))))

(defmethod glut:keyboard-up ((win my-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ((#\q #\Q #\Escape) t)))

(defun run-it ()
  (setf *the-window* (make-instance 'my-window))
  (glut:display-window *the-window*))
