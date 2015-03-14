;; paints a line to the canvas
;;
;; Usage (following (asdf:operate 'asdf:load-op :cl-canvas))
;;   (in-package :cl-canvas-line-example)
;;   (start-server)
;;   visit localhost:5000
;;
;; Notes
;;   - mi and setc are macros defined in ../src/canvas.lisp

(in-package :cl-canvas-example)

(defpackage cl-canvas-line-example
  (:use :cl :cl-canvas :cl-canvas-example :clack :cl-who :cl-ppcre))

(in-package :cl-canvas-line-example)

(defvar *server* nil)
(setf (cl-who:html-mode) :html5)

(setc can canvas :width 400 :context (mi context))
(setc sp can-point :x 51 :y 52)
(setc ep can-point :x 151 :y 52)
(setc line can-line :start-point sp :end-point ep)
(add-to-context can line)

(defun get-public-path ()
  (let ((path-string
         (cl-ppcre:regex-replace
          "\/cl-canvas\/.*"
          (namestring *default-pathname-defaults*)
          "/cl-canvas/examples/public/")))
    (pathname path-string)))

(defun index (env)
  `(200
    (:content-type "text/html")
     (,(with-html-output-to-string (str nil :prologue t :indent t)
       (:html
         (:head
          (:link :rel "stylesheet" :type "text/css" :href "/public/css/style.css"))
         (:body
          (:div :id "container"
                (:canvas :id "canvas")
                (:script
                 (str (canvas-to-string can))))))))))

(defun start-server ()
  (setf *server* (clack:clackup
                  (clack.builder:builder
                   (clack.middleware.static:<clack-middleware-static>
                    :path "/public/"
                    :root (get-public-path))
                   #'index))))

(defun stop-server ()
  (clack:stop *server*))
