;; paints a circle to the canvas
;;
;; Usage (following (asdf:operate 'asdf:load-op :cl-canvas))
;;   (in-package :cl-canvas-circle-example)
;;   (start-server index)
;;   visit localhost:5000
;;
;; (stop-server)
;;
;; Notes
;;   - mi and setc are macros defined in ../src/canvas.lisp

(in-package :cl-canvas-example)

(defpackage cl-canvas-circle-example
  (:use :cl :cl-canvas :cl-canvas-example :clack :cl-who :cl-ppcre))

(in-package :cl-canvas-circle-example)

(setf (cl-who:html-mode) :html5)

(setc can canvas :width 400 :context (mi context))
(setf circ (mi can-circle :center-point (mi can-point :x 70 :y 70)
                          :radius 50
                          :color "#33CCFF"
                          :fill-color "#66FF99"
                          :width 7))
(add-to-context can circ)

(setf index (index-maker can))
