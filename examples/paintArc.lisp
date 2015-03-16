;; paints a line to the canvas
;;
;; Usage (following (asdf:operate 'asdf:load-op :cl-canvas))
;;   (in-package :cl-canvas-arc-example)
;;   (start-server)
;;   visit localhost:5000
;;
;; Notes
;;   - mi and setc are macros defined in ../src/canvas.lisp

(in-package :cl-canvas-example)

(defpackage cl-canvas-arc-example
  (:use :cl :cl-canvas :cl-canvas-example :clack :cl-who :cl-ppcre))

(in-package :cl-canvas-arc-example)

(defvar *server* nil)
(setf (cl-who:html-mode) :html5)

(setc can canvas :width 400 :context (mi context))
(setf arc (mi can-arc :center-point (mi can-point :x 50 :y 50)
                      :radius 15
                      :width 5
                      :start-angle (mi angle :degrees -15)
                      :end-angle (mi angle :degrees 300)))
(add-to-context can arc)

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
