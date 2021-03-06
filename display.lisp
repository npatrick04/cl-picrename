(in-package :picrename)

(defparameter splash-image "/home/nick/2012-04-07-09.19.53.jpg")

(org.tfeb.hax.memoize:def-memoized-function get-jpeg-date-time (filename)
  (handler-bind ((jpegmeta::segment-marker-off #'jpegmeta::nil-after-invalid-segment))
    (jpegmeta:get-tiff-field
     (jpegmeta:read-jpeg-from-file filename)
     'jpegmeta::date-time)))

(defun load-from-list (win)
  "Load an image into openGL"
  (let ((filename (car *list-of-files*)))
    (when (cl-fad:file-exists-p filename)
      (setf (texture-id win)
            (il:with-init
                (ilut:renderer :opengl)
              (ilut:gl-load-image filename))
            <date> (let ((date-time (get-jpeg-date-time filename)))
                     (substitute #\- #\:
                                 (subseq date-time 0 (position #\Space date-time))))))))
                         
;;; 
;;; OPENGL Stuff
;;;

(defclass my-window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p)
   (texture-id :initform nil :accessor texture-id))
  (:default-initargs :width 400 :height 300
                     :title "Renamer"
                     :x 100 :y 100
                     :mode '(:double :rgb :depth)
                     :fullscreen nil
                     :tick-interval (round 1000 10)))  ; milliseconds per tick

(defmethod glut:tick ((win my-window))
  (when *running*
    (get-more-files)
    (glut:post-redisplay)))             ; tell GLUT to redraw

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

  (when *list-of-files*
    (load-from-list win))
  
  (unless (texture-id win)     ; load texture if needed
    (setf (texture-id win)
          (il:with-init
	    (ilut:renderer :opengl)
	    (ilut:gl-load-image splash-image))))

  (when (texture-id win)       ; enable texturing if we have one
    (gl:enable :texture-2d)))

(defparameter *space-between-lines* 15)
(defun glut-print (pos font text r g b a &optional (font-type 'stroke))
  "http://www.gamedeception.net/threads/1876-Printing-Text-with-glut
Print with glut to an x, y with a glut:font"
  (gl:with-pushed-matrix
    (gl:scale 0.1 0.1 0)
    (glut:stroke-width font 5)
    (let ((blending (gl:enabledp :blend))
          (x (car pos))
          (y (cadr pos)))
      (flet ((position-character (x y)
               (if (eq font-type 'bitmap)
                   (gl:raster-pos x y)
                   (gl:translate x y 0)))
             (print-char (font char)
               (if (eq font-type 'bitmap)
                   (glut:bitmap-character font char)
                   (glut:stroke-character font char))))
        (gl:enable :blend)
        (gl:color r g b a)
        (position-character x y)
        (map nil
             #'(lambda (c)
                 (if (eq c #\Newline)
                     (position-character x (decf y *space-between-lines*))
                     (print-char font
                                 (char-int c))))
             text)
        (unless blending
          (gl:disable :blend))))))

(defmethod glut:display ((win my-window))
  ;; clear the color buffer and depth buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:color 1 1 1 1)
  (gl:load-identity)              ; reset the modelview matrix

  (when (texture-id win)          ; bind the texture if we have it
    (gl:bind-texture :texture-2d (texture-id win)))
  
  (utilities:condlet
      (((fullscreen-p win)
        (height (glut:get :screen-height))
        (width (glut:get :screen-width)))
       (t
        (height (glut:height win))
        (width (glut:width win))))
    (gl:with-primitives :quads
      ;; front face
      (gl:tex-coord 0.0 1.0) (gl:vertex 0.0 height 0.0)
      (gl:tex-coord 1.0 1.0) (gl:vertex width  height  0.0)
      (gl:tex-coord 1.0 0.0) (gl:vertex width  0.0 0.0)
      (gl:tex-coord 0.0 0.0) (gl:vertex 0.0 0.0 0.0)))

  ;; Text overlay
  (bordeaux-threads:acquire-lock prompt-lock)
  (dolist (abuf *focus-list*)
    (let ((buf (cdr abuf)))
      (when (visible buf)
        (render buf))))
  (bordeaux-threads:release-lock prompt-lock)
  ;; swap the buffer onto the screen  
  (glut:swap-buffers))


(defmethod glut:reshape ((win my-window) width height)
  (gl:viewport 0 0 width height)  ; reset the current viewport
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix

  ;; set perspective based on window aspect ratio
  (glu:ortho-2d 0 width 0 height)

  ;; (format out "Win: Height: ~A, Width: ~A~%" (glut:height win) (glut:width win))
  ;; (format out "New: Height: ~A, Width: ~A~%" height width)

  (setf (glut:height win) height
  	(glut:width win) width)

  (gl:matrix-mode :modelview)     ; select the modelview matrix
                                        ; reset the matrix
  (gl:load-identity))


(defmethod glut:keyboard ((win my-window) key xx yy)
  (declare (ignore xx yy))

  ;; (princ "In the GLUT keyboard handler")
  ;; (fresh-line)
  
  ;; Check for special keys
  (multiple-value-bind (shift ctrl alt) (glut:get-modifier-values)
    (declare (ignore shift ctrl))
    (if alt                             ;; TODO, figure out why ctrl didn't work
	(case key
	  ((#\f #\F)
	   ;; save whether we're in fullscreen
	   (let ((full (fullscreen-p win)))
	     (glut:destroy-current-window)       ; close the current window
	     (glut:display-window   ; open a new window with fullscreen toggled
	      (make-instance 'my-window
			     :fullscreen (not full)))))
          ((#\l #\L) (progn (setf *list-of-files* (get-list-of-files))
			    (load-from-list win))))
	  
	;; No alt modifier, do normal stuff
        (progn ;;(format out "Getting ready to funcall a fn with key ~A.~%" key)
	  
	  (if-let (fn (gethash key (keymap *current-buffer*)))
                 (funcall fn)
                 (if-let (fn (gethash :default (keymap *current-buffer*)))
                   (funcall fn key)
                   (format out "No command for ~A" key)))))))

