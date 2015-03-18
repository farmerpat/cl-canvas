;; cl > js
;;
;; expects canvas tag with id "canvas"
;; see http://www.html5canvastutorials.com/tutorials/ for functionality to offer
;;
;; TODO
;;   - add README.md
;;   - add compile-to-file
;;   - allow &rest args for add-to-context
;;   - add docstrings
;;   - add error checking to class inits (e.g. (member given-weight '("bold" "normal" "italic")))
;;   - add stroke-color to can-text?
;;   - consider some kind of relative positioning scheme
;;     - for example, enabling the user to specify that an object is to be centered
;;       and the system knowing that (if the obj is a circle) that x = canvas.width / 2;
;;   - make a super class to hold common slots: preserve-context, color, etc
;;   - make a descendant of that for shapes (for lineWidth, etc)
;;   - add regular polygon class
;;   - add plotting?
;;   - ***BREAK OUT INDEX-MAKER AND START/STOP-SERVER INTO A FILE, EXAMPLE.LISP THAT
;;     ALL THE EXAMPLES DEPEND ON***
;;
;; NOTES
;;   - for animation class
;;     - (wrap-in-function anim js-fn)
;;     - (run-on-click anim id-of-dom-node)
;;     - (run-on-key-press anim key)
;;     - support #\C as arg with #'char-to-key-code
;;
(in-package :cl-canvas)

;; setclass
(defmacro setc (sym class &rest initargs)
  `(setf ,sym (make-instance ',class ,@initargs)))

(defmacro mi (class &rest initargs)
  `(make-instance ',class ,@initargs))

(defmacro write-with-pc-wrap ((obj str) &body body)
  `(with-output-to-string (,str)
     (let ((pc (get-preserve-context ,obj)))
       (if pc (format ,str "context.save();~%"))
       ,@body
       (if pc (format ,str "context.restore();~%")))))

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
           :accessor get-height)
   (tag-id :initarg :tag-id
       :initform "canvas"
       :accessor get-tag-id)))

(defmethod canvas-area-spec-string ((c canvas))
  (let ((width (write-to-string (get-width c)))
        (height (write-to-string (get-height c))))
    (with-output-to-string (str)
      (format str "var canvas = document.getElementById('~A');~%" (get-tag-id c))
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
    (loop for elt across (get-elements c)
       do
         (format str "~A~%" (element-to-string elt)))))

(defclass can-point ()
  ((x :initarg :x
      :initform 0
      :accessor get-x)
   (y :initarg :y
      :initform 0
      :accessor get-y)))

(defmethod to-param-string ((p can-point))
  (with-output-to-string (str)
    (let ((x (write-to-string (get-x p)))
          (y (write-to-string (get-y p))))
      (format str "~A, ~A" x y))))

(defclass can-text ()
  ((font-pt :initarg :font-pt
            :initform 12
            :accessor get-font-pt)
   (font-family :initarg :font-family
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
   ;; replace with point
   (x-pos :initarg :x-pos
          :initform 30
          :accessor get-x-pos)
   (y-pos :initarg :y-pos
          :initform 30
          :accessor get-y-pos)
   (preserve-context :initarg :preserve-context
                     :initform nil
                     :accessor get-preserve-context)))

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
  (write-with-pc-wrap (ct str)
    (format str "context.fillStyle = '~A';~%" (get-font-color ct))
    (format str "context.font = '~A';~%" (build-font-string ct))
    (format str "context.fillText(~A);~%" (build-fill-text-params ct))))

(defclass can-line ()
  ((start-point :initarg :start-point
                :initform (make-instance 'can-point)
                :accessor get-start-point)
   (end-point :initarg :end-point
              :initform (make-instance 'can-point)
              :accessor get-end-point)
   (width :initarg :width
          :initform 5
          :accessor get-width)
   (color :initarg :color
          :initform "#000000"
          :accessor get-color)
   (cap :initarg :cap
        :initform "butt"
        :accessor get-cap)
   (preserve-context :initarg :preserve-context
                     :initform nil
                     :accessor get-preserve-context)))

(defmethod element-to-string ((cl can-line))
  (write-with-pc-wrap (cl str)
    (format str "context.beginPath();~%")
    (format str "context.moveTo(~A);~%" (to-param-string (get-start-point cl)))
    (format str "context.lineTo(~A);~%" (to-param-string (get-end-point cl)))
    (format str "context.lineWidth = ~A;~%" (get-width cl))
    (format str "context.strokeStyle = '~A';~%" (get-color cl))
    (format str "context.lineCap = '~A';~%" (get-cap cl))
    (format str "context.stroke();~%")))

(defclass angle ()
  ((degrees :initarg :degrees
            :initform 0
            :accessor get-degrees)
   (radians :initarg :radians
            :accessor get-radians)))

(defmethod initialize-instance :after ((a angle) &rest args)
  ;; if user gave rads and degs (or just rads), assume they know what they're doing
  (if (equal :degrees (car args))
      (setf (get-radians a) (* (get-degrees a) 0.017453292519943))))

(defclass can-arc ()
  ((center-point :initarg :center-point
                 :initform (make-instance 'can-point)
                 :accessor get-center-point)
   (radius :initarg :radius
           :initform 1
           :accessor get-radius)
   (start-angle :initarg :start-angle
                :initform (mi angle :degrees 0)
                :accessor get-start-angle)
   (end-angle :initarg :end-angle
              :initform (mi angle :degrees 180)
              :accessor get-end-angle)
   (clockwise :initarg :clockwise
              :initform t
              :accessor get-clockwise)
   (color :initarg :color
          :initform "#000000"
          :accessor get-color)
   (width :initarg :width
          :initform 10
          :accessor get-width)
   (preserve-context :initarg :preserve-context
                     :initform nil
                     :accessor get-preserve-context)))

(defmethod element-to-string ((a can-arc))
  (write-with-pc-wrap (a str)
    (format str "context.beginPath();~%")
    (format str "context.arc(~A);~%" (build-arc-params a))
    (format str "context.lineWidth = ~A;~%" (get-width a))
    (format str "context.strokeStyle = '~A';~%" (get-color a))
    (format str "context.stroke();~%")))

(defmethod build-arc-params ((a can-arc))
  (with-output-to-string (str)
    (format str "~A, " (to-param-string (get-center-point a)))
    (format str "~A, " (get-radius a))
    (format str "~A, " (get-radians (get-start-angle a)))
    (format str "~A, " (get-radians (get-end-angle a)))
    (format str "~A" (if (get-clockwise a) "false" "true"))))

(defclass can-circle (can-arc)
  ((start-angle :initform (mi angle :degrees 0)
                :reader get-start-angle)
   (end-angle :initform (mi angle :degrees 360)
              :reader get-end-angle)
   (clockwise :initform t
              :reader get-clockwise)
   (fill-color :initarg :fill-color
               :initform "#000000"
               :accessor get-fill-color)))

(defmethod element-to-string ((c can-circle))
  (write-with-pc-wrap (c str)
    (format str "context.beginPath();~%")
    (format str "context.arc(~A);~%" (build-arc-params c))
    (format str "context.fillStyle = '~A';~%" (get-fill-color c))
    (format str "context.fill();~%")
    (format str "context.lineWidth = ~A;~%" (get-width c))
    (format str "context.strokeStyle = '~A';~%" (get-color c))
    (format str "context.stroke();~%")))

(defclass can-donut (can-circle)
  ())

(defmethod element-to-string ((d can-donut))
  (write-with-pc-wrap (d str)
    (format str "context.beginPath();~%")
    (format str "context.arc(~A);~%" (build-arc-params d))
    (format str "context.lineWidth = ~A;~%" (get-width d))
    (format str "context.strokeStyle = '~A';~%" (get-color d))
    (format str "context.stroke();~%")))
