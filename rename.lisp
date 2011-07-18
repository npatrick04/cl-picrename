;;;; Picture renamer

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
      (remove name <names>)
      (push name <names>)))

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
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (force-output *standard-input*)
  (read-line *query-io*))

(defun make-description ()
  (let* ((desc (prompt-read "Description or #0-#9 for previous descriptions"))
	(num (handler-case (parse-integer desc)
	       ('SIMPLE-ERROR nil))))
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
      (#\Newline (rename (car *list-of-files*) (compile-name)))
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

(format t "Input: ")
(loop for ch = (read-char)
      until (eq ch #\Escape)
      do (input-handler ch)
         (fresh-line)
         (format t "~a~%Input: " (compile-name)))