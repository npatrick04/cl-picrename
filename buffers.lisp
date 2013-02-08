(in-package :picrename)

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


(defun dump-keymap (keymap)
           (maphash #'(lambda (key val)
                            (princ key out)
                            (format out " -> ")
                            (princ val out)
                            (format out "~%"))
                    keymap))

;;;
;;;  Buffer and point type
;;;

(defclass focusable ()
  ((keymap :accessor keymap :initform (make-keymap) :initarg :keymap)))

(defclass buffer (focusable)
  ((rows :initform 1 :initarg :rows :accessor rows)
   (pos :initform '(10 10) :initarg :pos :accessor pos)
   (pt :initform 0 :initarg :pt :accessor pt)
   (text :initform " " :initarg :text :accessor text)
   (visible :initform t :initarg :visible :accessor visible)
   (font :initform nil :initarg :font :accessor font)
   (color :initform '(255 255 255 1) :initarg :color :accessor color)
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
          (t (setf (text buffer) (utilities:mkstr (subseq (text buffer) 0 current-pos)
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
          (utilities:mkstr (subseq (text buf) 0 (1- (pt buf)))
                 (subseq (text buf) (pt buf))))
    (decf (pt buf))
    (decf (text-length buf))))

(defmethod render ((buf buffer))
  (when (visible buf)
    (glut-print (pos buf)
                (font buf)
                (text buf)
                255 255 255 1)))

;;;
;;;  Prompt buffer
;;;

(defclass prompt-buffer (buffer)
  ((prompt :accessor prompt :initarg :prompt)))

(defmethod render ((buf prompt-buffer))
  (when (visible buf)
    (glut-print (pos buf)
                (font buf)
                (utilities:mkstr (prompt buf) (text buf))
                255 255 255 1)))

(defun clear (buffer)
  (setf (pt buffer) 0
        (text buffer) ""))

(defparameter *current-buffer* nil)
(defparameter *focus-list* '())

(defun change-buffer (to-buffer)
  (setf *current-buffer* to-buffer))

;;;
;;;  Modes
;;;

(defparameter *all-modes* '())
(defmacro add-mode (name keymap)
  `(progn (push (cons ',name ',keymap) *all-modes*)))

