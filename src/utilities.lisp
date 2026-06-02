
#| note to anybody reading this code:
| contact me or create issue on github if there is something i am doing wrong
| or should fix. i am new to ltk/tcl/tk so there may be things i should do in
| a different way.
|#

(in-package #:regex-visualizer)

#|
| -------------------- Communication with wish --------------------
| we need to be able to communicate with wish directly, since ltk doesn't
| (obviously) bind all the tk features
|#

(defun send-receive (format-string &rest format-args)
  "send data to wish and return response mapped to a data type"
  (apply #'ltk:format-wish
         (format nil "senddata [~A]" format-string)
         format-args)
  (ltk::read-data))

(defun send-receive-string (format-string &rest format-args)
  "send data to wish and return response as a string"
  (apply #'ltk:format-wish
         (format nil "senddatastring [~A]" format-string)
         format-args)
  (ltk::read-data))

#|
| -------------------- general --------------------
|#

(defun make-pos (line pos) (format nil "~D.~D" line pos))

#|
| -------------------- text  utilities --------------------
|#
(defgeneric remove-tag  (txt tag &key start end)
  (:documentation ""))
(defmethod remove-tag  ((txt ltk:text) tag &key (start "1.0") (end "end"))
  (ltk:format-wish "~A tag remove ~A ~A ~A"
                   (ltk:widget-path txt) tag start end))

(defgeneric set-tags (txt tags)
  (:documentation "configure tags for ltk:text"))
(defmethod set-tags ((txt ltk:text) tags)
  (loop for (c-tag . c-values) in tags
        do (apply #'ltk:tag-configure txt c-tag c-values)))

(defgeneric add-tag (txt tag &key start end)
  (:documentation ""))
(defmethod add-tag ((txt ltk:text) tag &key (start "1.0") (end "end"))
  (ltk:format-wish "~A tag add ~A ~A ~A"
                   (ltk:widget-path txt) tag start end))

; cool trick to get # of lines of text cool trick https://stackoverflow.com/questions/4609382/
(defgeneric lines (txt)
  (:documentation ""))
(defmethod lines ((txt ltk:text))
  (let ((data-back (send-receive "~A index end-1c" (ltk:widget-path txt))))
    (truncate data-back)))

(defgeneric get-text (txt &key start end remove-newline)
  (:documentation ""))

(defmethod get-text ((txt ltk:text) &key (start '(1 0)) (end '("end")) (remove-newline t))
  (unless (listp start) (setf start (list start)))
  (unless (listp end)   (setf end   (list end)))
  (send-receive-string "~A get ~{~A~^.~} ~{~A~^.~}~:[~;-1c~]"
                       (ltk:widget-path txt)
                       start
                       end
                       remove-newline
                       ))

(defun get-text-line (txt start &key (remove-newline t))
  (get-text txt :start (list start 0) :end (list (+ 1 start) 0) :remove-newline remove-newline))

(defun remove-tags (txt tags)
  (mapc #'(lambda (x) (
                       remove-tag txt (first x))) tags))

;(defun io-main () t){{{
;
;(defun main () t)
;
;(defclass text-field ()
;  ((text
;     :initarg :text
;     :initform nil
;     :accessor :text)
;   (widget
;     :initform nil
;     :accessor :wd)
;   (highlights
;     :initarg :highlights
;     :initform nil
;     :accessor :highlights)
;   (parent
;     :initarg :parent
;     :initform nil
;     :accessor :parent
;     )
;   ))
;
;;(defmethod initialize-size-pos-instance :after ((reg regex-field) &rest initargs)
;;
;;  (with-slots (text wd highlights) reg
;;    (setf wd (make-instance 'text ))
;;    (setf )
;;
;;    )
;;  )
;;
;;
;
;; recieve data back as a data type
;(defun send-receive (format-string &rest format-args)
;
;  (apply #'format-wish
;         (format nil "senddata ~A" format-string)
;         format-args)
;  (ltk::read-data))
;
;; recieve data back as a string
;(defun send-receive-string (format-string &rest format-args)
;  (apply #'format-wish
;         (format nil "senddatastring ~A" format-string)
;         format-args)
;  (ltk::read-data))
;
;(defun remove-added-newline (str)
;  (string-right-trim '(#\n) str))
;
;(defun remove-tag  (widget tag &key (start "1.0") (end "end"))
;  (format-wish "~A tag remove ~A ~A ~A"
;               (widget-path widget) tag start end))
;
;(defun remove-tags (widget tags)
;  (mapc #'(lambda (x) (remove-tag widget (first x))) tags))
;
;(defun set-tag (widget tag values)
;  (apply #'tag-configure widget tag values))
;
;(defun set-tags (widget tags)
;  (loop for (c-tag . c-values) in tags
;        do (set-tag widget c-tag c-values)))
;
;(defun add-tag (widget tag &key (start "1.0") (end "end"))
;  (format-wish "~A tag add ~A ~A ~A"
;               (widget-path widget) tag start end))
;
;
;#| cool trick to get # of lines of text
;| https://stackoverflow.com/questions/4609382/getting-the-total-number-of-lines-in-a-tkinter-text-widget
;|#
;(defmethod lines (widget)
;  (:documentation "Get lines of text from widget")
;  )
;(defmethod lines ((widget ltk:text))
;  (let ((data-back (send-receive "[~A index end-1c]" (widget-path widget))))
;    (truncate data-back)))
;
;(defun text-no-newline (widget &key (start "1.0") (end "end"))
;  (send-receive-string "[~A get ~A ~A-1c]" (widget-path widget) start end))
;
;(defun text-line (widget line &key (remove-newline t))
;  (send-receive-string "[~A get ~F ~F~A]" (widget-path widget)
;                       line (+ line 1)
;                       (if remove-newline "-1c" "")))
;
;(defun make-pos (line pos) (format nil "~D.~D" line pos))
;(defun remove-added-newline (str)
;  (string-right-trim '(#\n) str))}}}
