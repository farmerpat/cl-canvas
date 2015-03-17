;; paints a line to the canvas
;;
;; Usage (following (asdf:operate 'asdf:load-op :cl-canvas))
;;   (in-package :cl-canvas-line-example)
;;   (start-server index)
;;   visit localhost:5000
;;
;; (stop-server)
;;
;; Notes
;;   - mi and setc are macros defined in ../src/canvas.lisp

(in-package :cl-canvas-example)

(defpackage cl-canvas-line-example
  (:use :cl :cl-canvas :cl-canvas-example :clack :cl-who :cl-ppcre))

(in-package :cl-canvas-line-example)

(setf (cl-who:html-mode) :html5)

(setc can canvas :width 400 :context (mi context))
(setc sp can-point :x 51 :y 52)
(setc ep can-point :x 151 :y 52)
(setc line can-line :start-point sp :end-point ep)
(add-to-context can line)

(setf index (index-maker can))
