
#| note to anybody reading this code:
| contact me or create issue on github if there is something i am doing wrong
| or should fix. i am new to ltk/tcl/tk so there may be things i should do in
| a different way.
|#

(in-package #:regex-visualizer)
; {{{
;(defclass widget-with-label ()
;  ((framelabel
;     :initarg  :framelabel
;     :initform nil
;     )
;   (label-text
;     :initarg  :label-text
;     :initform ""
;     )
;   (widget
;     :initarg  :widget
;     :initform (make-instance 'ltk:widget)
;     )))
;
;(defmethod initialize-instance ((wl widget-with-label) &rest args) 
;  (setf ())
;
;  )
; }}}
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
| -------------------- grid --------------------
|#
(defun configure-opt (widget type opt-vals)
  "easily set values and options for:
  configure, grid-configure, grid-rowconfigure, grid-columnconfigure "
  (let ((type-func (case type
                     (:row           #'ltk:grid-rowconfigure)
                     ((:col :column) #'ltk:grid-columnconfigure)
                     (:grid          #'ltk:grid-configure)
                     (:configure     #'ltk:configure)
                     (t (error "type: '~A' not applicable to function grid-configure-opt" type)))))
    (if (eql type :configure)
        (loop for (opt value) on opt-vals by #'cddr
              do (funcall type-func widget opt value))
        (loop with pos = (first opt-vals)
              for (opt value) on (rest opt-vals) by #'cddr
              do (funcall type-func widget pos opt value)))))

(defun configure-opts (widget &rest opt_list)
  "call configure-opt continuously on widget with passed arguments"
  (loop for (type opt-vals) on opt_list by #'cddr
        do (configure-opt widget type opt-vals)))

#|
| -------------------- general --------------------
|#
(defun make-pos (line pos) (format nil "~D.~D" line pos))

(defun index-to-pos (str start end)
  (let* ((match-string  (subseq str start end))
         (before-string (or (when (> start 0) (subseq str 0 start)) ""))
         (last-newline-before  (position #\Newline before-string :from-end t))
         (last-newline-match   (position #\Newline match-string :from-end t))

         (init-lines    (+ 1 (or (count #\Newline before-string) 0)))
         (init-pos      (if last-newline-before (+ last-newline-before 1) 0))
         (end-pos       (if last-newline-match  (+ last-newline-match  1) 0))
         (lines         (or (count #\Newline match-string) 0)))
    (values
      (make-pos  init-lines           (- start init-pos))
      (make-pos  (+ init-lines lines) (- end start end-pos)))))

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
                       remove-newline))

(defun get-text-line (txt start &key (remove-newline t))
  (get-text txt
            :start (list start 0)
            :end   (list (+ 1 start) 0)
            :remove-newline remove-newline))

(defun remove-tags (txt tags)
  (mapc #'(lambda (x) (remove-tag txt (first x))) tags))

