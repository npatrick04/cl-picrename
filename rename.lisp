;;;; Picture renamer

(in-package :picrename)

(defvar *config-file*
  (sb-ext:native-pathname
   (utilities:mkstr (sb-posix:getenv "HOME") "/.picrename")))
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
(defparameter *lock* nil)

(defparameter *initial-keymap* nil)
(defparameter *named-keymap* nil)
(defparameter *naming-keymap* nil)
(defparameter *modify-keymap* nil)
(defparameter *description-keymap* nil)

(defun get-path-of (file)
  (if file
      (subseq file 0 (1+ (position #\/ file :from-end t)))
      "."))

(defmacro dbg (form)
  (let ((val (gensym)))
    `(let ((,val ,form))
       (format out "~A=~A~%" ',form ,val)
       ,val)))

(defun rename (file1 f2)
  "Rename file1 which includes the path, to the filename of fname2.
fname2 will be located in the same directory as file1"
  ;; (let* ((last/ (1+ (position #\/ file1 :from-end t)))
  ;;        (path (subseq file1 0 last/))
  ;;        (file2 (concatenate 'string path fname2)))
  (let ((f1 (pathname file1)))
    (if (cl-fad:file-exists-p f1)
        (if (cl-fad:file-exists-p f2)
            (error 'new-file-already-exists)
            (progn (format out "file 1 exists and 2 doesn't~%Renaming ~a to ~a~%" file1 (pathname-name f2))
                   (rename-file f1 (pathname-name f2))))
        (error 'original-file-doesnt-exist))))

(defun change-mode (mode)
  (format out "Changing mode to ~A~%" mode)
  (setf (keymap *current-buffer*) (symbol-value (cdr (assoc mode *all-modes*)))
        (text *fsm-state*) (string mode)))

(defun display-output-name ()
  (clear *output-name*)
  (let ((pathspec (compile-name (pathname (car *list-of-files*))
                                 ;; *default-pathname-defaults*
                                 )))
    (insert-content-at-point (pathname-name pathspec)
                             *output-name*)))

(defun toggle-name (name)
  (if (member name <names> :test #'equal)
      (setf <names> (delete name <names> :test #'equal))
      (push name <names>))
  (display-output-name))

(defun maybe-toggle-name (name)
  (toggle-name name)
  (if (or <names>
          (car <description>))
      (progn (display-output-name)
             (change-mode 'named))
      (set-initial-state)))

(defun get-list-of-files ()
  (mapcar #'sb-ext:native-namestring (cl-fad:list-directory
                                      #+LINUX "/home/nick/lisp/cl-picrename/examples"
                                      #+WIN32 "C:\\Users\\Nick\\Dropbox\\lisp-code\\cl-picrename\\examples\\"
                                      )))

(defvar *names-format* "~{~a~#[~; and ~:; ~]~}")
(defun compile-name (pathspec)
  (let ((root nil))
    (when <names> (setf root (format nil *names-format* (reverse <names>))))
    (when *description-in-progress*
      (setf root
            (string-trim " " (format nil "~@[~A ~]~A" root
                                     *description-in-progress*))))
    (when-let (the-desc (car <description>))
      (setf root
            (string-trim " " (format nil "~@[~A ~]~A" root
                                     the-desc))))
    (unless root
      (setf root " "))
    (let ((new-file (make-pathname :name (string-trim " " (format nil "~@[~A ~]~A" root <date>))
                                   :defaults pathspec)))
      (if (not (cl-fad:file-exists-p new-file))
          new-file
          (let (num-file)
            (do ((num 2 (1+ num)))
                ((and num-file
                      (not (cl-fad:file-exists-p num-file))) num-file)
              (setf num-file (make-pathname :name (string-trim " " (format nil "~@[~A ~]~@[~A ~]~A" root num <date>))
                                            :defaults pathspec))
              (format out "compile-name: num=~A,  num-file=~A~%" num num-file)))))))

(defun set-initial-state ()
  (clear *output-name*)
  (update-prompt "Enter Character for Name: ")
  (setf <names> nil
        *naming-key* nil)
  (change-mode 'initial))

(defun save-input-map (the-map)
  (with-open-file (rc *config-file*
                      :direction :output
                      :if-exists :supersede)
    (print (let ((all-entries '()))
             (maphash #'(lambda (key value)
                          (push (list key value) all-entries))
                      the-map)
             all-entries)
           rc)))

(defun read-input-map ()
  (let ((inputmap (make-hash-table :test #'eq)))
    (handler-case (with-open-file (rc *config-file*
                                      :direction :input
                                      :if-does-not-exist :error)
                    (mapc #'(lambda (set)
                              (setf (gethash (car set) inputmap)
                                    (cadr set)))
                          (read rc))
                    inputmap)
      (file-error () inputmap))))

(defun kill-picrename ()
  (clean-socket)
  (glut:destroy-current-window)
  ;; (when *lock*
  ;;   (close *lock*)
  ;;   (delete-file "picrename.lock"))
  )

(defun update-prompt (new-prompt)
  (bordeaux-threads:acquire-lock prompt-lock)
  (setf (prompt *prompt*) new-prompt)
  (clear *prompt*)
  (bordeaux-threads:release-lock prompt-lock))

;;; TODO Picture buffer...or content...or something

(defun initialize-buffers ()
  ;; (setf *default-font* glut:+bitmap-9-by-15+)
  (setf *default-font* glut:+stroke-roman+)

  (add-buffer *prompt*
              prompt-buffer
              :pos '(10 100)
              :prompt "prompt>")
  (add-buffer *fsm-state* buffer :pos '(10 300) :read-only t)
  (add-buffer *output-name* buffer :pos '(10 500))
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

(defun def-modify-keymap ()
  (let ((map (make-sparse-keymap)))
    (maphash #'(lambda (key val)
                 (add-command map key #'(lambda ()
                                          (revise-name key val))))
             *inputmap*)
    (add-command map #\Escape 'back-to-named-or-initial)
    (defparameter *modify-keymap* map)))

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

(defun def-initial-keymap ()
  (let ((map (make-sparse-keymap)))
    (maphash #'(lambda (key val)
                 (add-command map key #'(lambda ()
                                          (maybe-toggle-name val))))
             *inputmap*)
    (add-command map #\Escape 'kill-picrename)
    (add-command map #\' 'modify-entry)
    (add-command map #\; 'update-description)
    (add-command map :default 'start-naming)
    (defparameter *initial-keymap* map)))

;;;
;;;  Named Mode
;;; 

(defun name-the-picture ()
  (clear *output-name*)
  (let ((the-file (pop *list-of-files*)))
    (format out "rename ~A~%" the-file)
    ;; convert the file to a lisp pathtype
    (rename the-file (compile-name (pathname the-file))))
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

(defun def-named-keymap ()
  (let ((map (make-sparse-keymap)))
    (maphash #'(lambda (key val)
                 (add-command map key (lambda ()
                                        (maybe-toggle-name val))))
             *inputmap*)
    (add-command map #\Escape 'set-initial-state)
    (add-command map #\Return 'name-the-picture)
    (add-command map #\' 'modify-entry)
    (add-command map #\; 'update-description)
    (add-command map :default 'start-naming)
    (defparameter *named-keymap* map)))

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

(defun def-naming-keymap ()
  (let ((map (make-keymap)))
    (add-command map #\Escape 'back-to-named-or-initial)
    (add-command map #\Return 'finish-naming)
    (defparameter *naming-keymap* map)))

;;;
;;;  Describing Mode
;;; 

(defun exit-describing ()
  (push nil <description>)
  (update-prompt "Enter Character for Name: ")
  (if <names>
      (change-mode 'named)
      (set-initial-state)))

(defun finish-description ()
  (push (text *prompt*) <description>)
  (update-prompt "Enter Character for Name: ")
  (display-output-name)
  (if (or (car <description>) <names>)
      (change-mode 'named)
      (set-initial-state)))

(defun def-description-keymap ()
  (let ((map (make-keymap)))
    (add-command map #\Escape 'exit-describing)
    (add-command map #\Return 'finish-description)
    (defparameter *description-keymap* map)))

(add-mode description *description-keymap*)
(add-mode named *named-keymap*)
(add-mode naming *naming-keymap*)
(add-mode initial *initial-keymap*)
(add-mode modify *modify-keymap*)

;;; 
;;;  Entry points
;;;

(defun run-it ()
  (glut:init (lisp-implementation-type))
  (setf *list-of-files* 
        (mapcar #'(lambda (filename) (sb-ext:native-namestring filename))
                (cdr sb-ext:*posix-argv*))
        *name-in-progress* ""
	*the-window* (make-instance 'my-window)
        *running* t
        *inputmap* (read-input-map))

  ;; Update keymaps
  (def-modify-keymap)
  (def-initial-keymap)
  (def-named-keymap)
  (def-naming-keymap)
  (def-description-keymap)

  (initialize-buffers)
  (set-initial-state)
  (glut:display-window *the-window*))

;;; (defparameter the-thread (bordeaux-threads:make-thread #'run-it))

(defparameter *executable-scanner*
  (cl-ppcre:create-scanner "^picrename.exe" :multi-line-mode t))

(defun check-for-other-instances ()
  "Return true if an instance of the application is already running"
  (> (length (cl-ppcre:all-matches "picrename.exe" 
                                   (let ((tl (sb-ext:run-program "check-app.bat" nil :input t :output :stream :search t))
                                         (task-list '()))
                                     (do ((c (read-char (sb-ext:process-output tl))
                                             (read-char (sb-ext:process-output tl) nil 'the-end)))
                                         ((not (characterp c)) (coerce (nreverse task-list) 'string))
                                       (push c task-list)))))
     ;; The match always returns a match for the batch file echo, so check for more than 1 match
     2))


(defun main ()
  (handler-case (progn (create-receive-socket)
                       ;; (let ((*lock* (open "picrename.lock"
                       ;;                     :direction :output
                       ;;                     :if-exists :error)))
                       (glut:init (lisp-implementation-type))
                       (setf *list-of-files* 
                             (mapcar #'(lambda (filename) (sb-ext:native-namestring filename))
                                     (cdr sb-ext:*posix-argv*))
                             *name-in-progress* ""
                             *running* t
                             *the-window* (make-instance 'my-window)
                             *inputmap* (read-input-map))

                       ;; Update keymaps
                       (def-modify-keymap)
                       (def-initial-keymap)
                       (def-named-keymap)
                       (def-naming-keymap)
                       (def-description-keymap)
                       
                       (initialize-buffers)
                       (set-initial-state)
                       (glut:display-window *the-window*))
    (sb-bsd-sockets:address-in-use-error ()
      (sleep 3)
      (mapc #'(lambda (filename)
                (udp-send-file (sb-ext:native-namestring filename)))
            (cdr sb-ext:*posix-argv*)))))

;; Compile this file
(unless (member :swank *features*)
  (format t "Generating executable!~%")
  (sb-ext:save-lisp-and-die (sb-ext:native-namestring (utilities:mkstr (sb-posix:getcwd) "/picrename.exe"))
                            :toplevel #'main
                            :executable t))
