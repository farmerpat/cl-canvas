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
                :can-circle
                :can-donut
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
           :can-circle
           :can-donut
           :setc
           :mi
           :index-maker
           :start-server
           :stop-server
           :get-public-path
           :*server*))

(in-package :cl-canvas-example)

(defvar *server* nil)

(defun index-maker (can)
  #'(lambda (env)
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
                  (str (canvas-to-string can)))))))))))

(defun get-public-path ()
  (let ((path-string
         (cl-ppcre:regex-replace
          "\/cl-canvas\/.*"
          (namestring *default-pathname-defaults*)
          "/cl-canvas/examples/public/")))
    (pathname path-string)))

(defun start-server (index)
  (setf *server* (clack:clackup
                  (clack.builder:builder
                   (clack.middleware.static:<clack-middleware-static>
                    :path "/public/"
                    :root (get-public-path))
                   index))))

(defun stop-server ()
  (clack:stop *server*))
