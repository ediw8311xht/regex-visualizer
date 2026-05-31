
#|
| ---------- to-do ----------
| - add tree view of regex - `ppcre:parse-string`
| - add options for multiline-mode, extended-mode, and single-line-mode
| - match all occurrences on line (do-scans -> loop over scan)
| ---------- notes ----------
| panedwindow - resizable window
|#

(in-package #:regex-visualizer)


#| ----- main -----
| this is a huge function with a lot of smaller functions with side effects...
| i should break it up into parts and make more modular
|#

;(defun draw-main () nil)

(defun draw-main ()
  (ltk:with-ltk ()
    (let* ((content (make-instance 'ltk:frame))
           (regex-widget (make-instance 'ltk:text
                                        ;:height 20 :width 20
                                        :master content
                                        :wrap :none
                                        :state :normal
                                        :background "#AAAAAA"
                                        ))
           (visual-widget (make-instance 'ltk:text
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
           ;(multiline-mode nil)
           ;(extended-mode  nil)
           )
      (labels
        (
         ;(toggle-text-area (on-off)
         ;  (if (eq on-off :on)
         ;      (progn (ltk:configure regex-widget :state :normal)
         ;             (ltk:configure visual-widget :state :normal))
         ;      (progn (ltk:configure regex-widget :state :disabled)
         ;             (ltk:configure visual-widget :state :disabled))))
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
                (declare (ignore registers))
                (update-highlights-single v)))))
         (initialize-size-pos ()
           (ltk:grid-rowconfigure ltk:*tk* 0 :weight 1)
           (ltk:grid-columnconfigure ltk:*tk* 0 :weight 1)
           ; configure content padding and fill entire window
           ; 2 columns, 1 row (regex-widget visual-widget)
           (ltk:configure content :padding "12 12 12 12")
           (ltk:grid content 0 0 :columnspan 2 :rowspan 1 :sticky "nsew" )

           (ltk:grid-rowconfigure content 0 :weight 1)
           (ltk:grid-columnconfigure content 0 :weight 1)
           (ltk:grid-columnconfigure content 1 :weight 1)

           ; configure regex-widget and visual-widget to fill entire column
           (ltk:grid regex-widget 0 0 :sticky "nsew")
           (ltk:grid visual-widget 0 1 :sticky "nsew"))
         (main ()
               (initialize-size-pos)
               (set-tags visual-widget tags-highlights) ; set highlighting tags
               (set-tags regex-widget  tags-highlights) ; set highlighting tags
               ; to-do
               ; create separate function to create scanner only when <KeyRelease> occurs on regex-widget
               (ltk:bind regex-widget  "<KeyRelease>" #'text-change)
               (ltk:bind visual-widget "<KeyRelease>" #'text-change)
               ))
        (main)))))

