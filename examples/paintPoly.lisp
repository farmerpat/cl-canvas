;; paints a regular polygon to the canvas
;;
;; Usage (following (asdf:operate 'asdf:load-op :cl-canvas))
;;   (in-package :cl-canvas-poly-example)
;;   (start-server index)
;;   visit localhost:5000
;;
;; (stop-server)
;;
;; Notes
;;   - mi and setc are macros defined in ../src/canvas.lisp

(in-package :cl-canvas-example)

(defpackage cl-canvas-poly-example
  (:use :cl :cl-canvas :cl-canvas-example :clack :cl-who :cl-ppcre))

(in-package :cl-canvas-poly-example)

(setc can canvas :width 400 :context (mi context))
(setf poly (mi can-regular-polygon :center-point (mi can-point :x 50 :y 50) :side-length 40 :num-sides ))
(add-to-context can poly)

(setf index (index-maker can))
