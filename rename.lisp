;;;; Picture renamer

;; (in-package :cl-picrename)

(asdf:load-system "cl-opengl")   ; load OpenGL bindings
(asdf:load-system "cl-glu")      ; load GLU bindings
(asdf:load-system "cl-glut")     ; load GLUT bindings
(ql:quickload "cl-devil")
(ql:quickload "bordeaux-threads")
(ql:quickload "alexandria")
(use-package :alexandria)
(asdf:load-system "utilities")
(use-package :utilities)
(asdf:load-system :cl-fad)
(ql:quickload "lisp-magick")

(defvar *config-file* ".picrename")
(defvar *list-of-files* '())
(defvar *inputmap* (make-hash-table :test #'eq))
(defvar *name-in-progress* "")
(defvar <names> nil)
(defvar <description> (list))
(defvar *description-in-progress* "")
(defvar <date> nil)
(defparameter *prompt* "prompt>")
(defparameter out *standard-output*)
(defvar *running* t)
(defparameter overwrite-map-file t
  "if t, overwrite the *config-file* in $HOME with
the contents of the input map")
(defparameter *default-font* nil)
(defparameter *focus* nil)
(defparameter *naming-key* nil)

(defvar prompt-lock (bordeaux-threads:make-lock))

(defun get-path-of (file)
  (if file
      (subseq file 0 (1+ (position #\/ file :from-end t)))
      "."))

(defun rename (file1 fname2)
  "Rename file1 which includes the path, to the filename of fname2.
fname2 will be located in the same directory as file1"
  (let* ((last/ (1+ (position #\/ file1 :from-end t)))
         (path (subseq file1 0 last/))
         (file2 (concatenate 'string path fname2)))
    (if (cl-fad:file-exists-p file1)
        (if (cl-fad:file-exists-p file2)
            (error 'new-file-already-exists)
            (progn (format out "file 1 exists and 2 doesn't~%Renaming ~a to ~a~%" file1 file2)
                   (rename-file file1 file2)))
        (error 'original-file-doesnt-exist))))

(defun change-mode (mode)
  (setf (keymap *current-buffer*) (symbol-value (cdr (assoc mode *all-modes*))))
  (setf (text *fsm-state*) (string mode)))

(defun toggle-name (name)
  (if (member name <names> :test #'equal)
      (setf <names> (delete name <names> :test #'equal))
      (push name <names>))
  (display-output-name))

(defun maybe-toggle-name (name)
  (toggle-name name)
  (if (or <names>
          <description>)
      (progn (display-output-name)
             (change-mode 'named))
      (set-initial-state)))

(defun get-list-of-files ()
  (mapcar #'sb-ext:native-namestring (cl-fad:list-directory "/home/nick/lisp/cl-picrename/examples")))

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
                         
(defvar *names-format* "~{~a~#[~; and ~:; ~]~}")
(defun compile-name (path)
  (let ((root nil))
    (when <names> (setf root (format nil *names-format* (reverse <names>))))
    (when *description-in-progress*
      (setf root
            (string-trim " " (format nil "~A ~A" root
                                     *description-in-progress*))))
    (when-let (the-desc (car <description>))
      (setf root
            (string-trim " " (format nil "~A ~A" root
                                     the-desc))))
    (unless root
      (setf root " "))
    (let ((no-num-name (string-trim " " (format nil "~A ~A.jpg" root <date>))))
      (if (not (cl-fad:file-exists-p (format nil "~A/~A" path no-num-name)))
          no-num-name
          (do* ((num 2 (1+ num))
                (num-name (string-trim " " (format nil "~A ~A ~A.jpg" root num <date>))))
               ((not (cl-fad:file-exists-p num-name)) num-name))))))

(defun set-initial-state ()
  (clear *output-name*)
  (update-prompt "Enter Character for Name: ")
  (setf <names> nil
        *naming-key* nil)
  (change-mode 'initial))

(defmacro backspace (string)
  `(setf ,string
         (coerce (butlast (coerce ,string 'list))
                 'string)))

(defun save-input-map (the-map)
  (let ((home (sb-posix:getenv "HOME")))
    (with-open-file (rc (mkstr home "/" *config-file*)
                        :direction :output
                        :if-exists :supersede)
      (print (let ((all-entries '()))
               (maphash #'(lambda (key value)
                            (push (list key value) all-entries))
                        the-map)
               all-entries)
             rc))))

(defun read-input-map ()
  (let ((home (sb-posix:getenv "HOME"))
        (inputmap (make-hash-table :test #'eq)))
    (handler-case (with-open-file (rc (mkstr home "/" *config-file*)
                                      :direction :input
                                      :if-does-not-exist :error)
                    (mapc #'(lambda (set)
                              (setf (gethash (car set) inputmap)
                                    (cadr set)))
                          (read rc))
                    inputmap)
      (file-error () inputmap))))

(defun kill-picrename () (glut:destroy-current-window))

;;;
;;;  Keymap
;;;

(defmacro add-command (keymap key fn)
  `(setf (gethash ,key ,keymap) ,fn))

(defun make-sparse-keymap ()
  (make-hash-table :test #'eq))

(defun self-insert-character (char)
  (insert-content-at-point char *current-buffer*))

(defun make-keymap ()
  (let ((map (make-sparse-keymap)))
    (add-command map #\Backspace #'(lambda ()
                                     (delete-backwards *current-buffer*)))
    (add-command map :default 'self-insert-character)
    map))


(defclass focusable ()
  ((keymap :accessor keymap :initform (make-keymap) :initarg :keymap)))

;;;
;;;  Buffer and point type
;;;

(defclass buffer (focusable)
  ((rows :initform 1 :initarg :rows :accessor rows)
   (pos :initform '(10 10) :initarg :pos :accessor pos)
   (pt :initform 0 :initarg :pt :accessor pt)
   (text :initform " " :initarg :text :accessor text)
   (visible :initform t :initarg :visible :accessor visible)
   (font :initform nil :initarg :font :accessor font)
   (color :initform '(90 90 90 1) :initarg :color :accessor color)
   (mark :initform nil :accessor mark)
   (text-length :accessor text-length)
   (read-only :initform nil :initarg :read-only :reader read-only)))

(defmethod initialize-instance :after ((buf buffer) &key)
  (setf (text-length buf) (length (text buf))
        (pt buf) (text-length buf))
  (unless (font buf)
    (setf (font buf) *default-font*)))


(defmacro add-buffer (name type &rest rest)
  `(progn (defparameter ,name (make-instance ',type ,@rest))
          (if-let (the-entry (assoc ',name *focus-list*))
            (rplacd the-entry ,name)
            (push (cons ',name ,name) *focus-list*))))

(define-condition point-outside-of-buffer () ())
(define-condition point-negative () ())
(define-condition tried-to-insert-in-readonly-buffer () ())

(defun set-pt-to-end-of-buffer (buffer)
  (let ((content-length (length (text buffer))))
    (setf (text-length buffer) content-length
          (pt buffer) content-length)))

(defmethod insert-content-at-point ((content string) (buffer buffer))
  (if (read-only buffer)
      (error 'tried-to-insert-in-readonly-buffer)
      (let ((current-pos (pt buffer))
            (content-length (length content)))
        (cond
          ((minusp current-pos) (error 'point-negative))
          ((> current-pos (text-length buffer))
           (set-pt-to-end-of-buffer buffer)
           (insert-content-at-point content buffer))
          (t (setf (text buffer) (mkstr (subseq (text buffer) 0 current-pos)
                                        content
                                        (subseq (text buffer) current-pos)))
             (incf (pt buffer) (if (stringp content)
                                   content-length
                                   1))
             (incf (text-length buffer) content-length))))))

(defmethod insert-content-at-point ((ch character) (buffer buffer))
  (insert-content-at-point (string ch) buffer))

(defmethod insert-content-at-point ((sym symbol) (buffer buffer))
  (insert-content-at-point (string sym) buffer))

(defmethod delete-backwards ((buf buffer))
  (when (plusp (pt buf))
    (setf (text buf)
          (mkstr (subseq (text buf) 0 (1- (pt buf)))
                 (subseq (text buf) (pt buf))))
    (decf (pt buf))
    (decf (text-length buf))))

(defmethod render ((buf buffer))
  (when (visible buf)
    (glut-print (pos buf)
                (font buf)
                (text buf)
                90 90 90 1)))

;;;
;;;  Prompt buffer
;;;

(defclass prompt-buffer (buffer)
  ((prompt :accessor prompt :initarg :prompt)))

(defmethod render ((buf prompt-buffer))
  (when (visible buf)
    (glut-print (pos buf)
                (font buf)
                (mkstr (prompt buf) (text buf))
                90 90 90 1)))

(defun clear (buffer)
  (setf (pt buffer) 0
        (text buffer) ""))

(defun update-prompt (new-prompt)
  (bordeaux-threads:acquire-lock prompt-lock)
  (setf (prompt *prompt*) new-prompt)
  (clear *prompt*)
  (bordeaux-threads:release-lock prompt-lock))

;;; TODO Picture buffer...or content...or something

(defparameter *global-keymap* (make-sparse-keymap))
(defparameter *current-buffer* nil)
(defparameter *focus-list* '())

(defun change-buffer (to-buffer)
  (setf *current-buffer* to-buffer))

(defun initialize-buffers ()
  (setf *default-font* glut:+bitmap-9-by-15+)

  (add-buffer *prompt*
              prompt-buffer
              :pos '(10 10)
              :prompt "prompt>")
  (add-buffer *fsm-state* buffer :pos '(10 30) :read-only t)
  (add-buffer *output-name* buffer :pos '(10 50))
  (add-buffer *revise-assignments* buffer
              :pos '(400 400)
              :visible nil)
  (setf *current-buffer* *prompt*))
(initialize-buffers)

;;;
;;;  Finite State Machine
;;; 

(defun insert-name-from-map (c)
  (if-let (in-mapped (gethash c *inputmap*))
          (progn (toggle-name in-mapped)

                 :named)
          (progn (setf *naming-key* c)
                 (format out "Character ~A, beginning naming~%" c)
                 (update-prompt (format nil "Name (~A): " c))
                 :naming)))

(defun start-naming (char)
  (setf *naming-key* char)
  (update-prompt (format nil "Name (~A): " char))
  (change-mode 'naming))

(defun back-to-named-or-initial ()
  (setf *name-in-progress* ""
        (visible *revise-assignments*) nil)
  (update-prompt "Name: ")
  (display-output-name)
  (if <names>
      (change-mode 'named)
      (set-initial-state)))

(defun revise-name (key name)
  (setf *naming-key* key
        (visible *revise-assignments*) nil)
  (update-prompt (format nil "Name (~A): " key))
  (insert-content-at-point name *prompt*)
  (change-mode 'naming))

(let ((map (make-sparse-keymap)))
  (add-command map #\Escape 'back-to-named-or-initial)
  (maphash #'(lambda (key val)
               (add-command map key #'(lambda ()
                                        (revise-name key val))))
           *inputmap*)
  (defparameter *modify-keymap* map))

(defun modify-entry ()
  (setf (pos *revise-assignments*) (list (- (glut:height *the-window*)
                                           *space-between-lines*)
                                        (round (glut:width *the-window*)
                                               2)))
  (clear *revise-assignments*)
  (maphash #'(lambda  (key name)
               (insert-content-at-point (format nil "~A => ~A~%" key name) *revise-assignments*))
           *inputmap*)
  (setf (visible *revise-assignments*) t)
  (change-mode 'modify))

(defun dump-keymap (keymap)
           (maphash #'(lambda (key val)
                            (princ key out)
                            (format out " -> ")
                            (princ val out)
                            (format out "~%"))
                    keymap))

(defun add-name-to-input-maps (key name)
  (setf (gethash key *inputmap*) name)
  (mapc #'(lambda (map)
            (add-command map key #'(lambda ()
                                     (maybe-toggle-name name))))
        (list *initial-keymap*
              *named-keymap*)))

(let ((map (make-sparse-keymap)))
  (add-command map #\Escape 'kill-picrename)
  (add-command map #\' 'modify-entry)
  (maphash #'(lambda (key val)
               (add-command map key #'(lambda ()
                                        (maybe-toggle-name val))))
           *inputmap*)
  (add-command map :default 'start-naming)
  (defparameter *initial-keymap* map))

(defun name-the-picture ()
  (clear *output-name*)
  (let ((the-file (pop *list-of-files*)))
    (format out "rename ~A~%" the-file)
    (rename the-file (compile-name (get-path-of the-file))))
  (cond
    (*list-of-files*
     (format out "~%Current list of files: ~%~{  ~A~%~}" *list-of-files*)
     (load-from-list *the-window*)
     (set-initial-state))
    (t
     ;; Save the inputmap to $HOME/*config-file*
     (when overwrite-map-file (save-input-map *inputmap*))

     ;; and then kill us
     (setf *running* nil)
     (glut:destroy-current-window)
     :initial)))

(defun update-description ()
  (update-prompt "Enter Description: ")
  (change-mode 'description))

(let ((map (make-sparse-keymap)))
  (add-command map #\Escape 'set-initial-state)
  (add-command map #\Return 'name-the-picture)
  (add-command map #\' 'modify-entry)
  (add-command map #\; 'update-description)
  (maphash #'(lambda (key val)
               (add-command map key (lambda ()
                                      (maybe-toggle-name val))))
           *inputmap*)
  (add-command map :default 'start-naming)
  (defparameter *named-keymap* map))

(defun display-output-name ()
  (clear *output-name*)
  (insert-content-at-point (compile-name ".") *output-name*))


(defun finish-naming ()
  (push (text *prompt*) <names>)
  (add-name-to-input-maps *naming-key* (text *prompt*))
  (update-prompt "Enter Character for Name: ")
  (display-output-name)
  (change-mode 'named))

(let ((map (make-keymap)))
  (add-command map #\Escape 'back-to-named-or-initial)
  (add-command map #\Return 'finish-naming)
  (defparameter *naming-keymap* map))

(defun exit-describing ()
  (setf <description> nil)
  (update-prompt "Enter Character for Name: ")
  (if <names>
      (change-mode 'named)
      (set-initial-state)))

(defun finish-description ()
  (push (text *prompt*) <description>)
  (update-prompt "Enter Character for Name: ")
  (display-output-name)
  (if <names>
      (change-mode 'named)
      (set-initial-state)))

(let ((map (make-keymap)))
  (add-command map #\Escape 'exit-describing)
  (add-command map #\Return 'finish-description)
  (defparameter *description-keymap* map))

(defparameter *all-modes* '())
(defmacro add-mode (name keymap)
  `(progn (push (cons ',name ',keymap) *all-modes*)))

(add-mode description *description-keymap*)
(add-mode named *named-keymap*)
(add-mode naming *naming-keymap*)
(add-mode initial *initial-keymap*)
(add-mode modify *modify-keymap*)

(defparameter *the-window* nil)

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
                     :tick-interval (round 1000 60)))  ; milliseconds per tick

(defmethod glut:tick ((win my-window))
  (when *running* (glut:post-redisplay)))        ; tell GLUT to redraw

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
	    (ilut:gl-load-image "/home/nick/2012-04-07-09.19.53.jpg"))))

  (when (texture-id win)       ; enable texturing if we have one
    (gl:enable :texture-2d)))

(defparameter *space-between-lines* 15)
(defun glut-print (pos font text r g b a)
  "http://www.gamedeception.net/threads/1876-Printing-Text-with-glut
Print with glut to an x, y with a glut:font"
  (let ((blending (gl:enabledp :blend))
        (x (car pos))
        (y (cadr pos)))
    (gl:enable :blend)
    (gl:color r g b a)
    (gl:raster-pos x y)
    (map nil
	 #'(lambda (c)
	     (if (eq c #\Newline)
                 (gl:raster-pos x (decf y *space-between-lines*))
                 (glut:bitmap-character font
                                        (char-int c))))
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
  
  (condlet
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

  (format out "Win: Height: ~A, Width: ~A~%" (glut:height win) (glut:width win))
  (format out "New: Height: ~A, Width: ~A~%" height width)

  (setf (glut:height win) height
  	(glut:width win) width)

  (gl:matrix-mode :modelview)     ; select the modelview matrix
                                        ; reset the matrix
  (gl:load-identity))


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
        (progn (if-let (fn (gethash key (keymap *current-buffer*)))
                 (funcall fn)
                 (if-let (fn (gethash :default (keymap *current-buffer*)))
                   (funcall fn key)
                   (format out "No command for ~A" key)))))))

;;; 
;;;  Entry points
;;;

(defun run-it ()
  (setf *name-in-progress* ""
	*the-window* (make-instance 'my-window)
        *running* t
        *inputmap* (read-input-map))
  (initialize-buffers)
  (set-initial-state)
  (glut:display-window *the-window*))

;;; (defparameter the-thread (bordeaux-threads:make-thread #'run-it))

(defun main ()
  (glut:init (lisp-implementation-type))
  (setf *list-of-files*
        (mapcar #'(lambda (filename) (mkstr (sb-posix:getcwd) "/" filename))
                  (cdr sb-ext:*posix-argv*))
        *name-in-progress* ""
	*the-window* (make-instance 'my-window)
        *inputmap* (read-input-map))
  (initialize-buffers)
  (set-initial-state)
  (glut:display-window *the-window*))

;; Compile this file
(unless (member :swank *features*)
  (sb-ext:save-lisp-and-die (mkstr (sb-posix:getcwd) "/picrename")
                            :toplevel #'main
                            :executable t))
