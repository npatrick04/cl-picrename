;;;; Picture renamer

(in-package :cl-picrename)

;; (ql:quickload "cl-devil")
;; (ql:quickload "bordeaux-threads")
;; (ql:quickload "alexandria")
(use-package :alexandria)
;; (asdf:load-system "utilities")
;; (use-package :utilities)
;; (asdf:load-system :cl-fad)
;; (ql:quickload "lisp-magick")

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
(defparameter *the-window* nil)

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

(defun display-output-name ()
  (clear *output-name*)
  (insert-content-at-point (compile-name ".") *output-name*))

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

(defun save-input-map (the-map)
  (let ((home (sb-posix:getenv "HOME")))
    (with-open-file (rc (utilities:mkstr home "/" *config-file*)
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
    (handler-case (with-open-file (rc (utilities:mkstr home "/" *config-file*)
                                      :direction :input
                                      :if-does-not-exist :error)
                    (mapc #'(lambda (set)
                              (setf (gethash (car set) inputmap)
                                    (cadr set)))
                          (read rc))
                    inputmap)
      (file-error () inputmap))))

(defun kill-picrename () (glut:destroy-current-window))

(defun update-prompt (new-prompt)
  (bordeaux-threads:acquire-lock prompt-lock)
  (setf (prompt *prompt*) new-prompt)
  (clear *prompt*)
  (bordeaux-threads:release-lock prompt-lock))

;;; TODO Picture buffer...or content...or something

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

(defun start-naming (char)
  (setf *naming-key* char)
  (update-prompt (format nil "Name (~A): " char))
  (change-mode 'naming))

(defun back-to-named-or-initial ()
  "Return the state of the system to named or initial without changing existing names"
  (setf *name-in-progress* ""
        (visible *revise-assignments*) nil)
  (update-prompt "Name: ")
  (display-output-name)
  (if <names>
      (change-mode 'named)
      (set-initial-state)))

;;;
;;;  Modification mode
;;; 

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

;;;
;;;  Initial Mode
;;;

(let ((map (make-sparse-keymap)))
  (add-command map #\Escape 'kill-picrename)
  (add-command map #\' 'modify-entry)
  (maphash #'(lambda (key val)
               (add-command map key #'(lambda ()
                                        (maybe-toggle-name val))))
           *inputmap*)
  (add-command map :default 'start-naming)
  (defparameter *initial-keymap* map))

;;;
;;;  Named Mode
;;; 

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
     (kill-picrename))))

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

;;;
;;;  Naming Mode
;;; 

(defun add-name-to-input-maps (key name)
  (setf (gethash key *inputmap*) name)
  (mapc #'(lambda (map)
            (add-command map key #'(lambda ()
                                     (maybe-toggle-name name))))
        (list *initial-keymap*
              *named-keymap*)))

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

;;;
;;;  Describing Mode
;;; 

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

(add-mode description *description-keymap*)
(add-mode named *named-keymap*)
(add-mode naming *naming-keymap*)
(add-mode initial *initial-keymap*)
(add-mode modify *modify-keymap*)

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
        (mapcar #'(lambda (filename) (utilities:mkstr (sb-posix:getcwd) "/" filename))
                  (cdr sb-ext:*posix-argv*))
        *name-in-progress* ""
	*the-window* (make-instance 'my-window)
        *inputmap* (read-input-map))
  (initialize-buffers)
  (set-initial-state)
  (glut:display-window *the-window*))

;; Compile this file
(unless (member :swank *features*)
  (sb-ext:save-lisp-and-die (utilities:mkstr (sb-posix:getcwd) "/picrename")
                            :toplevel #'main
                            :executable t))
