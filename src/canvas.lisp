;; cl > js
;;
;; expects canvas tag with id "canvas"
;; see http://www.html5canvastutorials.com/tutorials/ for functionality to offer
;;
;; TODO
;;   - add README.md
;;   - allow &rest args for add-to-context
;;   - add docstrings
;;   - add error checking to class inits (e.g. (member given-weight '("bold" "normal" "italic")))
;;   - integrate global *fill-color* for default or override?
;;   - add stroke-color to can-text?
;;     (allowing (with-canvas-string ...))
;;     (we probably dont even need defgeneric to allow this macro)
;;
(in-package :cl-canvas)

;; setclass
(defmacro setc (sym class &rest initargs)
  `(setf ,sym (make-instance ',class ,@initargs)))

(defmacro mi (class)
  `(make-instance ',class))

(defun can-line (line)
  (format nil "~A~%" line))

(defgeneric add-to-context (class-or-context element))
(defgeneric element-to-string (elt))

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
  ((elements :initarg :elements
             :initform (make-array 0 :fill-pointer 0 :adjustable t)
             :accessor get-elements)))

(defmethod add-to-context ((c context) elt)
  (vector-push-extend elt (get-elements c)))

(defmethod pop-context ((c context))
  (let ((elts (get-elements c)))
    (if (zerop (length elts))
        (format t "POP-CONTEXT refuses to pop...the context vector is empty~%")
        (vector-pop (get-elements c)))))

(defmethod context-to-string ((c context))
  (with-output-to-string (str)
    (format str "var context = canvas.getContext('2d');~%")
    (loop for elt being the elements of (get-elements c)
       do
         (format str "~A~%" (element-to-string elt)))))

(defclass can-text ()
  ((font-pt :initarg :font-pt
            :initform 12
            :accessor get-font-pt)
   (font-family :initarg :font-familiy
                :initform "Courier"
                :accessor get-font-family)
   (weight :initarg :weight
           :initform "normal"
           :accessor get-font-weight)
   (color :initarg :color
          :initform "black"
          :accessor get-font-color)
   (text        :initarg :text
                :initform "lorem ipsum"
                :accessor get-text)
   (x-pos :initarg :x-pos
          :initform 0
          :accessor get-x-pos)
   (y-pos :initarg :y-pos
          :initform 0
          :accessor get-y-pos)))

(defmethod build-font-string ((ct can-text))
  (with-output-to-string (str)
    (format str "~A " (get-font-weight ct))
    (format str "~Apt " (get-font-pt ct))
    (format str "~A" (get-font-family ct))))

(defmethod build-fill-text-params ((ct can-text))
  (with-output-to-string (str)
    (format str "'~A', " (get-text ct))
    (format str "~A, " (get-x-pos ct))
    (format str "~A" (get-y-pos ct))))

(defmethod element-to-string ((ct can-text))
  (with-output-to-string (str)
    (format str "context.fillStyle = '~A';~%" (get-font-color ct))
    (format str "context.font = '~A';~%" (build-font-string ct))
    (format str "context.fillText(~A);~%" (build-fill-text-params ct))))
