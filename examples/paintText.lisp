;; paints a string to the canvas
;;
;; to run following (asdf:operate 'asdf:load-op :cl-canvas)
;; (in-package :cl-canvas-example)
;; (start-server)
;; ;; visit localhost:5000

(in-package :cl-canvas-example)

(defvar *server* nil)
(setf (cl-who:html-mode) :html5)

(defun index (env)
  `(200
    (:content-type "text/html")
     (,(with-html-output-to-string (str nil :prologue t)
       (:html
         (:head
          (:link :rel "stylesheet" :type "text/css" :href "/public/css/style.css"))
         (:body
          (:div :id "container"
                (:canvas :id "canvas")
                ;; insert example canvas here, after the method accepts stream argument
                (esc (test str))
                (:script
                 "console.log('der');"))))))))

(defun test (strm)
  (with-output-to-string (strm)
  (format strm "derp~%")))

(defun start-server ()
  (setf *server* (clack:clackup
                  (clack.builder:builder
                   (clack.middleware.static:<clack-middleware-static>
                    :path "/public/"
                    :root (merge-pathnames "public/" *default-pathname-defaults* ))
                   #'index))))

(defun stop-server ()
  (clack:stop *server*))
