(in-package :cl-user)

(defpackage :cl-canvas-asd
  (:use :cl :asdf))

(in-package :cl-canvas-asd)

(defsystem :cl-canvas
  :version "0.0.1"
  :author "Patrick Connelly"
  :depends-on ("clack" "cl-who" "cl-ppcre")
  :serial t
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "canvas" :depends-on ("package"))))
               (:module "examples"
                        :depends-on ("src")
                        :components
                        ((:file "package")
                         (:file "paintText" :depends-on ("package"))
                         (:file "paintLine" :depends-on ("package")))))
  :description "Indirectly interact with the html5 canvas element via common lisp")
