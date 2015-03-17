;; paints a donut to the canvas
;;
;; Usage (following (asdf:operate 'asdf:load-op :cl-canvas))
;;   (in-package :cl-canvas-donut-example)
;;   (start-server index)
;;   visit localhost:5000
;;
;; (stop-server)
;;
;; Notes
;;   - mi and setc are macros defined in ../src/canvas.lisp

(in-package :cl-canvas-example)

(defpackage cl-canvas-donut-example
  (:use :cl :cl-canvas :cl-canvas-example :clack :cl-who :cl-ppcre))

(in-package :cl-canvas-donut-example)

(setf (cl-who:html-mode) :html5)

(setc can canvas :width 400 :context (mi context))
(setf don (mi can-donut :center-point (mi can-point :x 70 :y 70)
                        :radius 50
                        :color "#FFCC66"
                        :width 7))
(add-to-context can don)

(setf index (index-maker can))
