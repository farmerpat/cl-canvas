;; cl > js
;; trailing newline fn? probs not
;;
;; see http://www.html5canvastutorials.com/tutorials/ for functionality to offer

;; so tired of typing this out...not sure about the name tho
(defmacro make (sym class)
  `(setf ,sym (make-instance ',class)))

(defun can-line (line)
  (format nil "~A~%" line))

(defclass canvas ()
  ((context :initarg :context
            :accessor :get-context)))

(defmethod canvas-to-string ((c canvas))
  (let ((str ""))
    (setf str (concatenate 'string
                           (can-line "var canvas = document.getElementById('canvas');")
                           str))
    str))

(defclass context ()
  (()))
