(in-package :picrename)

(defvar *port* 8889)
(defparameter *socket* nil)

(defun create-receive-socket ()
  (setf *socket* (usocket:socket-connect nil nil :protocol :datagram
                                         :local-host "127.0.0.1"
                                         :local-port *port*)))

;; (defun start-server ()
;;   (multiple-value-bind (thread socket)
;;       (usocket:socket-server "127.0.0.1" 0 #'identity nil
;; 			     :in-new-thread t
;; 			     :protocol :datagram)
;;     (setq *echo-server* thread
;; 	  *port* (usocket:get-local-port socket))))

(defparameter *max-buffer-size* 5000)

(defvar *send-buffer*
  (make-array *max-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0))

(defvar *receive-buffer*
  (make-array *max-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0 :fill-pointer t))

(defun clean-buffers ()
  (fill *send-buffer* 0)
  (fill *receive-buffer* 0))

(defun clean-socket ()
  (usocket:socket-close *socket*))

(defun udp-send-file (file-string)
  (usocket:with-connected-socket (sock (usocket:socket-connect "127.0.0.1"
                                                               *port*
                                                               :protocol :datagram))
    (format t "Sending udp filename: ~A~%" file-string)
    (clean-buffers)
    (replace *send-buffer*
             (mapcar #'char-code (coerce file-string 'list)))
    (format t "Sent ~A bytes~%" (usocket:socket-send sock *send-buffer* (length file-string)))
    ))

(defun check-for-more-files ()
  (unless *socket*
    (create-receive-socket))
  (clean-buffers)
  (let ((received-files nil))
    (loop for sock = (car (usocket:wait-for-input (list *socket*) :timeout 0.1))
       while (usocket::state sock)
       do (multiple-value-bind (rbuf rlength) (usocket:socket-receive *socket* *receive-buffer* nil)
            (declare (ignorable rbuf))
            (setf (fill-pointer *receive-buffer*) rlength)
            ;; (format  t "socket-receive (state=~A, length=~A):~A~%" (usocket::state sock) rlength *receive-buffer*)
            (push (babel:octets-to-string
                    *receive-buffer*)
                   received-files)
            ;; (format t "When checking for files, got ~A~%" received-files)
            (setf (fill-pointer *receive-buffer*) 5000)
            )
       finally (return (nreverse received-files)))))

(defun get-more-files ()
  (when-let (files (check-for-more-files))
    ;; (format out "Additional file(s) to rename: ~A~%" files)
    (appendf *list-of-files* files)))

