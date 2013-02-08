;;;; cl-picrename.asd

(asdf:defsystem #:cl-picrename
  :name "picrename"
  :author "Nicholas Patrick <npatrick04@gmail.com>"
  :version "0.1"
  :maintainer "Nicholas Patrick <npatrick04@gmail.com>"
  :license "BSD"
  :description "JPEG picture renaming tool"
  :long-description ""
  :depends-on (cl-opengl cl-glu cl-glut cl-devil bordeaux-threads
                         alexandria utilities cl-fad jpegmeta babel
                         usocket cl-ppcre memoize)
  :components ((:file "package")
               (:file "socket-transfer" :depends-on ("package"))
               (:file "buffers" :depends-on ("package"))
               (:file "display" :depends-on ("package" "buffers" "socket-transfer"))
               (:file "rename" :depends-on ("package" "buffers" "display" "socket-transfer"))))

