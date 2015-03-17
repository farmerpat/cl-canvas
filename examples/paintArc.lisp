;; paints an arc to the canvas
;;
;; Usage (following (asdf:operate 'asdf:load-op :cl-canvas))
;;   (in-package :cl-canvas-arc-example)
;;   (start-server index)
;;   visit localhost:5000
;;
;; (stop-server)
;;
;; Notes
;;   - mi and setc are macros defined in ../src/canvas.lisp

(in-package :cl-canvas-example)

(defpackage cl-canvas-arc-example
  (:use :cl :cl-canvas :cl-canvas-example :clack :cl-who :cl-ppcre))

(in-package :cl-canvas-arc-example)

(setf (cl-who:html-mode) :html5)

(setc can canvas :width 400 :context (mi context))
(setf arc (mi can-arc :center-point (mi can-point :x 50 :y 50)
                      :radius 15
                      :width 5
                      :start-angle (mi angle :degrees -15)
                      :end-angle (mi angle :degrees 300)))
(add-to-context can arc)

(setf index (index-maker can))
