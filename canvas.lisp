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

;; so tired of typing this out...not sure about the name tho
;; setclass
(defmacro setc (sym class)
  `(setf ,sym (make-instance ',class)))

(defmacro mi (class)
  `(make-instance ',class))

;; destructively string append
(defmacro setsap (place content)
  `(setf ,place (concatenate 'string
                             ,place
                             ,content)))

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
  (let ((str "")
        (width (write-to-string (get-width c)))
        (height (write-to-string (get-height c))))
    (setsap str (can-line "var canvas = document.getElementById('canvas');"))
    (setsap str "canvas.width='")
    (setsap str width)
    (setsap str (can-line "';"))
    (setsap str "canvas.height='")
    (setsap str height)
    (setsap str (can-line "';"))))

(defmethod canvas-to-string ((c canvas))
  (let ((str ""))
    (setsap str (can-line (canvas-area-spec-string c)))
    (setsap str (can-line (context-to-string (get-context c))))
    str))

(defmethod add-to-context ((c canvas) element)
  (add-to-context (get-context c) element))

(defclass context ()
  ())

(defmethod context-to-string ((c context))
  (let ((str ""))
    (setf str (concatenate 'string
                           (can-line "var context = canvas.getContext('2d');")))
    str))

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

;; unify all of these to-strings with defgeneric
;; for good happy success???
(defmethod can-text-to-string ((t can-text))
  )
