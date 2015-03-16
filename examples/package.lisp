(in-package :cl-user)

(defpackage cl-canvas-example
  (:use :cl :cl-canvas :clack :cl-who :cl-ppcre)
  (:import-from :cl-canvas
                :canvas
                :context
                :add-to-context
                :canvas-to-string
                :can-text
                :can-point
                :can-line
                :can-arc
                :angle
                :setc
                :mi)
  (:export :canvas
           :context
           :add-to-context
           :canvas-to-string
           :can-text
           :can-point
           :can-line
           :can-arc
           :angle
           :setc
           :mi))
