
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
(defun make-pos (line row)
  "get the tcl/tk formatted position for a line and row
  remember: line is 1-indexed, row is 0 indexed"
  (format nil "~D.~D" line row))


(defun index-to-pos (str &rest pos-list &aux (str-len (length str)))
  (labels
    ((rec-make-pos (str line row index pos remaining)
       (cond
         ((not pos ) nil)

         ((>= index pos)
          (cons (make-pos line row) (rec-make-pos str line row index
                                                  (first remaining)
                                                  (rest  remaining))))
         ((>= index str-len)
          (make-list (+ 1 (length remaining)) :initial-element (make-pos line (+ row 1))))

         (t
          (let* ((is-newline (char= (aref str index) #\Newline))
                 (new-line   (if is-newline (+ line 1) line))
                 (new-row    (if is-newline 0 (+ row 1))))
            (rec-make-pos str new-line new-row (+ 1 index) pos remaining))))))
    (rec-make-pos str 1 0 0 (first pos-list) (rest pos-list))))

#|
| -------------------- text  utilities --------------------
|#
(defgeneric set-text-read-only (txt form)
  (:documentation "Set text for text widget that shouldn't be edited by user."))

(defmethod set-text-read-only ((txt ltk:text) (form function))
  (funcall form txt))

(defmethod set-text-read-only ((txt ltk:text) (form string))
  (setf (ltk:text txt) form))

(defmethod set-text-read-only :around ((txt ltk:text) form)
  (ltk:configure txt :state :normal)
  (call-next-method)
  (ltk:configure txt :state :disabled))

(defgeneric remove-tag  (txt tag &key start end)
  (:documentation "remove tag from text widget"))
(defmethod remove-tag  ((txt ltk:text) tag &key (start "1.0") (end "end"))
  (ltk:format-wish "~A tag remove ~A ~A ~A"
                   (ltk:widget-path txt) tag start end))

(defgeneric set-tags (txt tags)
  (:documentation "configure tags for ltk:text"))
(defmethod set-tags ((txt ltk:text) tags)
  (loop for (c-tag . c-values) in tags
        do (apply #'ltk:tag-configure txt c-tag c-values)))

(defgeneric add-tag (txt tag &key start end)
  (:documentation "add tag to text in text widget"))
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

