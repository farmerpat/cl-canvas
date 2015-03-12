;; cl > js
;;
;; expects canvas tag with id "canvas"
;; see http://www.html5canvastutorials.com/tutorials/ for functionality to offer
;;
;; TODO
;;   - add docstrings
;;   - defgeneric for to-strings??
;;     (allowing (with-canvas-string ...))
;;     (we probably dont even need defgeneric to allow this macro)
;;

;; setclass
(defmacro setc (sym class)
  `(setf ,sym (make-instance ',class)))

(defmacro mi (class)
  `(make-instance ',class))

(defun can-line (line)
  (format nil "~A~%" line))

(defgeneric add-to-context (class-or-context element))

(defclass canvas ()
  ((context :initarg :context
            :initform (mi context)
            :accessor get-context)
   (width :initarg :width
          :initform 200
          :accessor get-width)
   (height :initarg :height
           :initform 200
           :accessor get-height)))

(defmethod canvas-area-spec-string ((c canvas))
  (let ((width (write-to-string (get-width c)))
        (height (write-to-string (get-height c))))
    (with-output-to-string (str)
      (format str "var canvas = document.getElementById('canvas');~%")
      (format str "canvas.width='~A';~%" width)
      (format str "canvas.height='~A';~%" height))))

(defmethod canvas-to-string ((c canvas))
  (with-output-to-string (str)
    (format str "~A~%" (canvas-area-spec-string c))
    (format str "~A~%" (context-to-string (get-context c)))))

(defmethod add-to-context ((c canvas) element)
  (add-to-context (get-context c) element))

(defclass context ()
  ())

(defmethod context-to-string ((c context))
  (with-output-to-string (str)
    (format str "var context = canvas.getContext('2d');~%")))

(defmethod add-text-to-context ((c context) (t can-text))
  )

(defmethod add-to-context ((c context) element)
  )

(defclass can-text ()
  ((font-pt :initarg :font-pt
            :initform 12
            :accessor get-font-pt)
   (font-family :initarg :font-familiy
                :initform "Courier New"
                :accessor get-font-family)
   (text        :initarg :text
                :initform "lorem ipsum"
                :accessor get-text)))

(defmethod can-text-to-string ((t can-text))
  )
