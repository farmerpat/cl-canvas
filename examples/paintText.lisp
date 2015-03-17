;; paints a string to the canvas
;;
;; Usage (following (asdf:operate 'asdf:load-op :cl-canvas))
;;   (in-package :cl-canvas-text-example)
;;   (start-server index)
;;   visit localhost:5000
;;
;; (stop-server)
;;
;; Notes
;;   - mi and setc are macros defined in ../src/canvas.lisp

(in-package :cl-canvas-example)

(defpackage cl-canvas-text-example
  (:use :cl :cl-canvas :cl-canvas-example :clack :cl-who :cl-ppcre))

(in-package :cl-canvas-text-example)

(setf (cl-who:html-mode) :html5)

(setc can canvas :width 400 :context (mi context))
(add-to-context can (mi can-text
                        :text "make me do something interesting"
                        :font-family "Verdana"
                        :preserve-context t))

(setf index (index-maker can))
