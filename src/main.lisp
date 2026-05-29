
#|
| ---------- to-do ----------
| - add tree view of regex - `ppcre:parse-string`
| - add options for multiline-mode, extended-mode, and single-line-mode
| - match all occurrences on line (do-scans -> loop over scan)
| ---------- notes ----------
| panedwindow - resizable window
|#

(in-package #:regex-visualizer)

(defun remove-added-newline (str)
  (string-right-trim '(#\n) str))

(defun remove-tag  (widget tag &key (start "1.0") (end "end"))
  (format-wish "~A tag remove ~A ~A ~A"
               (widget-path widget) tag start end))

(defun remove-tags (widget tags)
  (mapc #'(lambda (x) (remove-tag widget (first x))) tags))

(defun set-tag (widget tag values)
  (apply #'tag-configure widget tag values))

(defun set-tags (widget tags)
  (loop for (c-tag . c-values) in tags
        do (set-tag widget c-tag c-values)))

(defun add-tag (widget tag &key (start "1.0") (end "end"))
  (format-wish "~A tag add ~A ~A ~A"
               (widget-path widget) tag start end))

; recieve data back as a data type
(defun send-receive (format-string &rest format-args)

  (apply #'format-wish
         (format nil "senddata [~A]" format-string)
         format-args)
  (ltk::read-data))

; recieve data back as a string
(defun send-receive-string (format-string &rest format-args)
  (apply #'format-wish
         (format nil "senddatastring [~A]" format-string)
         format-args)
  (ltk::read-data))

#| cool trick to get # of lines of text
| https://stackoverflow.com/questions/4609382/getting-the-total-number-of-lines-in-a-tkinter-text-widget
|#
(defun lines (widget)
  (let ((data-back (send-receive "~A index end-1c" (widget-path widget))))
    (truncate data-back)))

(defun text-no-newline (widget &key (start "1.0") (end "end"))
  (send-receive-string "~A get ~A ~A-1c" (widget-path widget) start end))

(defun text-line (widget line &key (remove-newline t))
  (send-receive-string "~A get ~F ~F~A" (widget-path widget) 
                       line (+ line 1)
                       (if remove-newline "-1c" "")))

(defun make-pos (line pos) (format nil "~D.~D" line pos))

#| ----- main -----
| this is a huge function with a lot of smaller functions with side effects...
| i should break it up into parts and make more modular
|#
(defun draw-main ()
  (with-ltk ()
    (let* ((content (make-instance 'frame))
           (regex-widget (make-instance 'text
                                        ;:height 20 :width 20
                                        :master content
                                        :wrap :none
                                        :state :normal
                                        :background "#AAAAAA"
                                        ))
           (visual-widget (make-instance 'text
                                         ;:height 20 :width 20
                                         :master content
                                         :wrap :none
                                         :state :normal
                                         :background "#AAAAAA"))
           (tags-highlights
             '(
               ("match" . (:background :blue   :foreground :white))
               ("group" . (:background :green  :foreground :white))
               ("error" . (:background :red    :foreground :white))
               ))
           (visual-lines     1)
           (multiline-mode nil)
           (extended-mode  nil)
           )
      (labels
        (
         (toggle-text-area (on-off)
           (if (eq on-off :on)
               (progn (configure regex-widget :state :normal)
                      (configure visual-widget :state :normal))
               (progn (configure regex-widget :state :disabled)
                      (configure visual-widget :state :disabled))))
         (set-match-highlights (line match-start match-end)
           (add-tag visual-widget "match" :start (make-pos line match-start) :end (make-pos line match-end))
           )
         (set-groups-highlights (line groups-start groups-end)
           (loop for start across groups-start
                 for end   across groups-end
                 do (add-tag visual-widget "group" :start (make-pos line start) :end (make-pos line end))))
         (update-highlights-single (scanner)
           (loop for line from 1 to (+ 1 visual-lines)
                 for line-text = (text-line visual-widget line)
                 do 
                 ;(format t "reg - ~A~%line - ~A~%" (text-no-newline regex-widget) line-text)
                 (multiple-value-bind (match-start match-end groups-start groups-end) (ppcre:scan scanner line-text)
                      (when match-start
                        (set-match-highlights line match-start match-end)
                        (set-groups-highlights line groups-start groups-end)))))
         (text-change (event)
           (declare (ignore event))

           (remove-tags visual-widget tags-highlights)
           (remove-tags regex-widget  tags-highlights)
           (setf visual-lines (lines visual-widget))
           (let ((regex-text (text-no-newline regex-widget)))
             (handler-case (ppcre:create-scanner regex-text)
               (error (c)
                      (progn (add-tag regex-widget "error")
                             (format t "Error in regex: /~A/" c)))
               (:no-error (v registers)
                (update-highlights-single v)))))
         (initialize-size-pos ()
           (grid-rowconfigure *tk* 0 :weight 1)
           (grid-columnconfigure *tk* 0 :weight 1)
           ; configure content padding and fill entire window
           ; 2 columns, 1 row (regex-widget visual-widget)
           (configure content :padding "12 12 12 12")
           (grid content 0 0 :columnspan 2 :rowspan 1 :sticky "nsew" )

           (grid-rowconfigure content 0 :weight 1)
           (grid-columnconfigure content 0 :weight 1)
           (grid-columnconfigure content 1 :weight 1)

           ; configure regex-widget and visual-widget to fill entire column
           (grid regex-widget 0 0 :sticky "nsew")
           (grid visual-widget 0 1 :sticky "nsew"))
         (main ()
               (initialize-size-pos)
               (set-tags visual-widget tags-highlights) ; set highlighting tags
               (set-tags regex-widget  tags-highlights) ; set highlighting tags
               ; to-do
               ; create separate function to create scanner only when <KeyRelease> occurs on regex-widget
               (bind regex-widget  "<KeyRelease>" #'text-change)
               (bind visual-widget "<KeyRelease>" #'text-change)
               ))
        (main)))))




;(draw-main)
;(defun io-main () t)
;
;(defun main () t)
;
#|
(defclass text-field ()
  ((text
     :initarg :text
     :initform nil
     :accessor :text)
   (widget
     :initform nil
     :accessor :wd)
   (highlights
     :initarg :highlights
     :initform nil
     :accessor :highlights)
   (parent
     :initarg :parent
     :initform nil
     :accessor :parent
     )
   ))

(defmethod initialize-size-pos-instance :after ((reg regex-field) &rest initargs)

  (with-slots (text wd highlights) reg
    (setf wd (make-instance 'text ))
    (setf )

    )
  )
|#


