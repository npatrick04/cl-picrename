;;;; Picture renamer

;;(in-package :cl-picrename)

(ql:quickload "cl-opengl")   ; load OpenGL bindings
(ql:quickload "cl-glu")      ; load GLU bindings
(ql:quickload "cl-glut")     ; load GLUT bindings
(ql:quickload "cl-devil")
(ql:quickload "bordeaux-threads")
(ql:quickload "closer-mop")
(ql:quickload "alexandria")
(use-package :alexandria)
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

(defun rename (file1 fname2)
  "Rename file1 which includes the path, to the filename of fname2.
fname2 will be located in the same directory as file1"
  (let* ((last/ (1+ (position #\/ file1 :from-end t)))
         (path (subseq file1 0 last/))
         (fname1 (subseq file1 last/))
         (file2 (concatenate 'string path fname2)))
    (if (cl-fad:file-exists-p file1)
        (if (cl-fad:file-exists-p file2)
            (error 'new-file-already-exists)
            (progn (format out "file 1 exists and 2 doesn't~%Renaming ~a to ~a~%" file1 file2)
                   (rename-file file1 file2)))
        (error 'original-file-doesnt-exist))))

(defun toggle-name (name)
  (if (member name <names> :test #'equal)
      (setf <names> (delete name <names> :test #'equal))
      (push name <names>)))

(defun get-list-of-files ()
  (mapcar #'sb-ext:native-namestring (cl-fad:list-directory "/home/nick/lisp/cl-picrename/examples")))

(defun words-equalp (c1 c2)
  (eq (the-char c1) (the-char c2)))

(defvar *names-format* "~{~a~#[~; and ~:; ~]~}")
(defvar *title-format-desc* "~A ~a num ~A")
(defvar *title-format-no-desc* "~A num ~A")
(defun compile-name ()
  (let ((names (format nil *names-format*
		       <names>)))
    (cond
      (*description-in-progress* (format nil *title-format-desc* names *description-in-progress* <date>))
      (<description> (format nil *title-format-desc* names (car <description>) <date>))
      (t (format nil *title-format-no-desc* names <date>)))))

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

(defmacro defrenamestate (fsm-type state &body body)
  `(fsm:defstate ,fsm-type ,state (fsm c)
     (case c
       ,@body)))
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defun set-initial-state (the-fsm)
  (setf *prompt* "Enter Character for Name: "
        <names> nil
	*name-in-work* ""
        (the-key the-fsm) nil)
  :initial)

(defmacro backspace (string)
  `(setf ,string
         (coerce (butlast (coerce ,string 'list))
                 'string)))

(defun update-prompt (new-prompt)
  (bordeaux-threads:acquire-lock prompt-lock)
  (setf *prompt* new-prompt)
  (bordeaux-threads:release-lock prompt-lock))
    
(fsm:deffsm input-fsm ()
  ((the-key :accessor the-key)
   (back :accessor back)))

(defrenamestate input-fsm :initial
  (#\; :describe)
  (#\' (update-prompt "Select an entry to modify: ")
       (setf (back fsm) :initial)
       :modify)
  (otherwise
   (if-let (in-mapped (gethash c *inputmap*))
     (progn (toggle-name in-mapped)
            :named)
     (progn (setf (the-key fsm) c)
            (format out "Character ~A, beginning naming~%" c)
            (setf *prompt* (format nil "Name (~A): " c))
            :naming))))

(defrenamestate input-fsm :named
  (#\Escape (set-initial-state fsm))
  (#\Return ; Finish the name of the file
   (rename (pop *list-of-files*) (compile-name))
   (cond
     (*list-of-files*
      (format out "~%Current list of files: ~%~{  ~A~%~}" *list-of-files*)
      (load-from-list *the-window*)
      (set-initial-state fsm))
     (t (glut:destroy-current-window)
        :initial)))
  (#\' (update-prompt "Select an entry to modify: ")
       (setf (back fsm) :named)
       :modify)
  (#\; (setf *prompt* "Enter Description")
       :describe)
  (otherwise
   (let ((in-mapped (gethash c *inputmap*)))
     (cond
       (in-mapped (toggle-name in-mapped)
                  (format out "Character ~A in map~%" c)
                  ;; Have to check if we've turned off a name
                  (if (or <names>
                          <description>)
                      :named
                      (set-initial-state fsm)))
       (t (setf (the-key fsm) c)
          (format out "Character ~A, beginning naming~%" c)
          (setf *prompt* (format nil "Name (~A): " c))
          :naming)))))


(defrenamestate input-fsm :naming
  (#\Escape (setf *name-in-progress* "")
            (update-prompt "Name: ")
            (if <names>
                :named
                (set-initial-state fsm)))
  (#\Return (format out "~%Character ~A set to ~A~%" (the-key fsm) *name-in-progress*)
            (push *name-in-progress* <names>)
            (setf (gethash (the-key fsm) *inputmap*) *name-in-progress*)
            (setf *name-in-progress* "")
            (update-prompt "Name: ")
            :named)
  (#\Backspace (backspace *name-in-progress*)
               (unless (string= "" *name-in-progress*)
                 (backspace *prompt*))
               :naming)
  (t (setf *name-in-progress*
           (concatenate 'string *name-in-progress* (list c)))
     (format out "~A" c)
     (update-prompt (concatenate 'string *prompt* (list c)))
     :naming))

(defrenamestate input-fsm :modify
  (#\Escape (back fsm)))

(defrenamestate input-fsm :describe
  (#\Escape (setf <description> nil)
            (if <names>
                :named
                (set-initial-state fsm)))
  (#\Return (push *description-in-progress* <description>)
            (setf *description-in-progress* nil)
            (if <names>
                :named
                (set-initial-state fsm)))
  (t (setf *description-in-progress*
           (concatenate 'string *description-in-progress* (list c)))
     :describe))

(defparameter *the-fsm* (make-instance 'input-fsm))
(setf *the-state* "INITIAL")

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
  (glut-print 10 30 glut:+bitmap-9-by-15+
	      (format nil "~A" (fsm:state *the-fsm*)) 90 90 90 1)
  (glut-print 10 10 glut:+bitmap-9-by-15+
	      *prompt* 90 90 90 1)
  (bordeaux-threads:release-lock prompt-lock)
  (if (not (eq (fsm:state *the-fsm*) :initial))
      (glut-print 10 50 glut:+bitmap-9-by-15+
		  (compile-name) 90 90 90 1))
  ;; swap the buffer onto the screen  
  (glut:swap-buffers))

(defmethod glut:reshape ((win my-window) width height)
  (gl:viewport 0 0 width height)  ; reset the current viewport
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix

  ;; set perspective based on window aspect ratio
  (glu:ortho-2d 0 width 0 height)

  (format out "Win: Height: ~A, Width: ~A~%" (glut:height win) (glut:width win))
  (format out "New: Height: ~A, Width: ~A~%" height width)

  (setf (glut:height win) height
  	(glut:width win) width)

  (gl:matrix-mode :modelview)     ; select the modelview matrix
                                        ; reset the matrix
  (gl:load-identity))

(defun load-from-list (win)
  (setf (texture-id win)
	(il:with-init
	  (ilut:renderer :opengl)
	  (ilut:gl-load-image (car *list-of-files*)))
        <date> (let ((wand (lisp-magick:new-magick-wand)))
                 (lisp-magick:magick-read-image wand (car *list-of-files*))
                 (let* ((date-time (lisp-magick:magick-get-image-property wand "date:modify"))
                        (date (subseq date-time 0 (position #\T date-time))))
                   (lisp-magick:clear-magick-wand wand)
                   date))))
                         

(defmethod glut:keyboard ((win my-window) key xx yy)
  (declare (ignore xx yy))
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
	(case key
	  ((#\Escape) (if (eql (fsm:state *the-fsm*) :initial)
			  (glut:destroy-current-window)
			  (funcall *the-fsm* key)))
	  (otherwise
	   (funcall *the-fsm* key))))))

(defun run-it ()
  (setf *name-in-progress* ""
	*prompt* "Enter Character for Name: "
	*the-window* (make-instance 'my-window))
  (set-initial-state *the-fsm*)
  (glut:display-window *the-window*))

(defun main ()
  (format out "Command line arguments: ~%~{~A~%~}" sb-ext:*posix-argv*)
  (setf *name-in-progress* ""
	*prompt* "Enter Character for Name: "
	*the-window* (make-instance 'my-window))
  (set-initial-state *the-fsm*)
  (glut:display-window *the-window*))

;; Compile this file
(unless (member :swank *features*)
  (sb-ext:save-lisp-and-die "/home/nick/lisp/cl-picrename/picrename"
                            :toplevel #'main
                            :executable t))
