
#|
| ---------- to-do ----------
| - add tree view of regex - `ppcre:parse-string`
| - add options for multiline-mode, extended-mode, and single-line-mode
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
           ; ---------- left-side ----------
           (left-frame    (make-instance 'ltk:frame :master content))
           (regex-widget-label (make-instance 'ltk:label 
                                              :master left-frame
                                              :takefocus 0
                                              :text "regex:"
                                              :foreground :green
                                              :background :black))

           (regex-widget (make-instance 'ltk:text
                                        ;:height 20 :width 20
                                        :master left-frame
                                        :wrap :none
                                        :state :normal
                                        :background "#AAAAAA"
                                        ))
           ; ---------- right-side ----------
           (right-frame   (make-instance 'ltk:frame :master content))
           (visual-widget-label (make-instance 'ltk:label 
                                              :master right-frame
                                              :takefocus 0
                                              :text "target text:"
                                              :foreground :orange
                                              :background :black))
           (visual-widget (make-instance 'ltk:text
                                         ;:height 20 :width 20
                                         :master right-frame
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
         (initialize-size-pos ()
           (ltk:grid-rowconfigure    ltk:*tk* 0 :weight 1)
           (ltk:grid-columnconfigure ltk:*tk* 0 :weight 1)
           ; configure content padding and fill entire window
           ; 2 columns, 1 row [left-frame right-frame]
           (ltk:configure            content     :padding "12 12 12 12")
           (ltk:configure            left-frame  :padding "2 2 2 2")
           (ltk:configure            right-frame :padding "2 2 2 2")

           ; 2 cols 1 row
           (ltk:grid   content       0 0 :columnspan 2 :rowspan 1 :sticky "nsew" )
           ; 1 col 3 rows
           (ltk:grid   left-frame    0 0 :sticky "nsew" :columnspan 1 :rowspan 3)
           (ltk:grid   right-frame   0 1 :sticky "nsew" :columnspan 1 :rowspan 3)

           (ltk:grid   regex-widget-label  0 0 :sticky "nsew" )
           (ltk:grid   regex-widget        1 0 :sticky "nsew" )
           (ltk:grid   visual-widget-label 0 0 :sticky "nsew" )
           (ltk:grid   visual-widget       1 0 :sticky "nsew" )

           (ltk:grid-rowconfigure    content     0 :weight 1)
           (ltk:grid-columnconfigure content     0 :weight 1)
           (ltk:grid-columnconfigure content     1 :weight 1)

           (ltk:grid-rowconfigure    left-frame  0 :weight 1)
           (ltk:grid-rowconfigure    left-frame  1 :weight 5)
           (ltk:grid-rowconfigure    left-frame  2 :weight 3)

           (ltk:grid-columnconfigure left-frame  0 :weight 1)

           (ltk:grid-rowconfigure    right-frame 0 :weight 1)
           (ltk:grid-rowconfigure    right-frame 1 :weight 5)
           (ltk:grid-rowconfigure    right-frame 2 :weight 3)

           (ltk:grid-columnconfigure right-frame 0 :weight 1)
           )
         (set-match-highlights (line match-start match-end)
           (add-tag visual-widget "match" :start (make-pos line match-start) :end (make-pos line match-end))
           )
         (set-groups-highlights (line groups-start groups-end)
           (loop for start across groups-start
                 for end   across groups-end
                 do (add-tag visual-widget "group" :start (make-pos line start) :end (make-pos line end))))
         (update-highlights-single (scanner)
           (loop for line from 1 to (+ 1 visual-lines)
                 for line-text = (get-text-line visual-widget line)
                 do 
                 (ppcre:do-scans (match-start match-end groups-start groups-end scanner line-text)
                  ;multiple-value-bind (match-start match-end groups-start groups-end) (ppcre:do-scans scanner line-text)
                      (when match-start
                        (set-match-highlights line match-start match-end)
                        (set-groups-highlights line groups-start groups-end)))))
         (text-change (event)
           (declare (ignore event))

           (remove-tags visual-widget tags-highlights)
           (remove-tags regex-widget  tags-highlights)
           (setf visual-lines (lines visual-widget))
           (let ((regex-text (get-text regex-widget)))
             (handler-case (ppcre:create-scanner regex-text)
               (error (c)
                      (progn (add-tag regex-widget "error")
                             (format t "Error in regex: /~A/" c)))
               (:no-error (v registers)
                (declare (ignore registers))
                (update-highlights-single v)))))
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

         ;(toggle-text-area (on-off)
         ;  (if (eq on-off :on)
         ;      (progn (ltk:configure regex-widget :state :normal)
         ;             (ltk:configure visual-widget :state :normal))
         ;      (progn (ltk:configure regex-widget :state :disabled)
         ;             (ltk:configure visual-widget :state :disabled))))
