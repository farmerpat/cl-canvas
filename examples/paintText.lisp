;; paints a string to the canvas
;;
;; to run following (asdf:operate 'asdf:load-op :cl-canvas)
;; (in-package :cl-canvas-example)
;; (start-server)
;; ;; visit localhost:5000

(in-package :cl-canvas-example)

(defvar *server* nil)
(setf (cl-who:html-mode) :html5)

(setf can (make-instance 'canvas :width 400 :context (make-instance 'context)))
(add-to-context can (make-instance 'can-text
                                   :text "make me do something interesting"
                                   :font-family "Verdana"))

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
