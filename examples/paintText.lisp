;; paints a string to the canvas
;;
;; to run following (asdf:operate 'asdf:load-op :cl-canvas)
;; (in-package :cl-canvas-example)
;; (start-server)
;; ;; visit localhost:5000

(in-package :cl-canvas-example)

(defvar *server* nil)
(defvar *test-root-path* *default-pathname-defaults*)
(setf (cl-who:html-mode) :html5)

(setf can (make-instance 'canvas :context (make-instance 'context)))
(add-to-context can (make-instance 'can-text :text "make me do something interesting"))

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
                 (str (canvas-to-string can)))
                (:script
                 "console.log('der');"))))))))

(defun start-server ()
  (setf *server* (clack:clackup
                  (clack.builder:builder
                   (clack.middleware.static:<clack-middleware-static>
                    :path "/public/"
                    :root (merge-pathnames "public/" *test-root-path*))
                   #'index))))

(defun stop-server ()
  (clack:stop *server*))
