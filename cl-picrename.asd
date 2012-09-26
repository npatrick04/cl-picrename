;;;; cl-picrename.asd
;; (asdf:load-system "cl-opengl")   ; load OpenGL bindings
;; (asdf:load-system "cl-glu")      ; load GLU bindings
;; (asdf:load-system "cl-glut")     ; load GLUT bindings

(asdf:defsystem #:cl-picrename
  :serial t
  :depends-on (cl-opengl cl-glu cl-glut cl-devil bordeaux-threads
                         alexandria utilities lisp-magick cl-fad)
  :components ((:file "package")
               (:file "buffers")
               (:file "display")
               (:file "rename")))

