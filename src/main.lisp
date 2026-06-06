
#|
| ---------- to-do ----------
| - different colors for groups
| - add tree view of regex - ppcre:parse-string
| - show results as strings
| - hover functionality for groups and matches
| ---------- notes ----------
| panedwindow - resizable window
|#

(in-package #:regex-visualizer)


#| ----- main -----
| this is a huge function with a lot of smaller functions with side effects...
| i should break it up into parts and make more modular
|#

(defun draw-main ()
  (ltk:with-ltk ()
    (let* ((content (make-instance 'ltk:frame))
           ; ---------- left-side ----------
           (left-frame
             (make-instance 'ltk:frame :master content))
           (regex-widget-label
             (make-instance 'ltk:label
                            :master left-frame
                            :takefocus 0
                            :text "regex:"
                            :foreground :green
                            :background :black))

           (regex-widget
             (make-instance 'ltk:text
                            ;:height 20 :width 20
                            :master left-frame
                            :wrap :none
                            :state :normal
                            :background "#AAAAAA"
                            ))
           (buttons
             (make-instance 'ltk:frame :master left-frame)
             )
           (multi-line-mode-button
             (make-instance 'ltk:check-button
                            :master buttons
                            :text   "multi-line-mode"
                            ))
           (extended-mode-button
             (make-instance 'ltk:check-button
                            :master buttons
                            :text   "extended-mode"
                            ))
           ; ---------- right-side ----------
           (right-frame
             (make-instance 'ltk:frame :master content))
           (visual-widget-label
             (make-instance 'ltk:label
                            :master right-frame
                            :takefocus 0
                            :text "target text:"
                            :foreground :orange
                            :background :black))
           (visual-widget
             (make-instance 'ltk:text
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
           (multi-line-mode nil)
           (extended-mode  nil)
           )
      (labels
        (
         (initialize-size-pos ()
           (ltk:grid   content                0 0 :sticky "nsew" :columnspan 2 :rowspan 1)
           ; left side
           (ltk:grid   left-frame             0 0 :sticky "nsew" :columnspan 1 :rowspan 3)
           (ltk:grid   visual-widget-label    0 0 :sticky "nsew" )
           (ltk:grid   visual-widget          1 0 :sticky "nsew" )
           (ltk:grid   buttons                2 0 :sticky "nsew" :columnspan 2 :rowspan 1)
           (ltk:grid   multi-line-mode-button 0 0 :sticky "nsew")
           (ltk:grid   extended-mode-button   0 1 :sticky "nsew")
           ; right side
           (ltk:grid   right-frame            0 1 :sticky "nsew" :columnspan 1 :rowspan 3)
           (ltk:grid   regex-widget-label     0 0 :sticky "nsew" )
           (ltk:grid   regex-widget           1 0 :sticky "nsew" )

           (configure-opts ltk:*tk*
                           :row '(0 :weight 1)
                           :col '(0 :weight 1)
                           )
           (configure-opts content
                           :configure '(:padding "12 12 12 12")
                           :row       '(0 :weight 1)
                           :col       '(0 :weight 1)
                           :col       '(1 :weight 1)
                           )
           (configure-opts left-frame
                           :configure '(:padding "2 2 2 2")
                           :row       '(0 :weight 0 :minsize 50)
                           :row       '(1 :weight 1 :minsize 100)
                           :row       '(2 :weight 0 :minsize 50)
                           :col       '(0 :weight 1)
                           )
           (configure-opts right-frame
                           :configure '(:padding "2 2 2 2")
                           :row       '(0 :weight 0 :minsize 50)
                           :row       '(1 :weight 1 :minsize 100)
                           :row       '(2 :weight 0 :minsize 50)
                           :col       '(0 :weight 1)
                           )
           )
         
         ; multi line
         (set-match-highlights-multi (txt match-start match-end)
           (multiple-value-bind (start end) (index-to-pos txt match-start match-end)
             (add-tag visual-widget "match" :start start :end end)))
         (set-groups-highlights-multi (txt groups-start groups-end)
           (loop for s across groups-start
                 for e   across groups-end
                 do (multiple-value-bind (start end) (index-to-pos txt s e)
                      (add-tag visual-widget "group" :start start :end end))))
         (update-highlights-multi-line (scanner) 
           (let ((txt (get-text visual-widget))) 
             (ppcre:do-scans (match-start match-end groups-start groups-end scanner txt)
               (when match-start
                 (set-match-highlights-multi txt match-start match-end)
                 (set-groups-highlights-multi txt groups-start groups-end)))))

         ; single line
         (set-match-highlights-single (line match-start match-end)
           (add-tag visual-widget "match" :start (make-pos line match-start) :end (make-pos line match-end)))
         (set-groups-highlights-single (line groups-start groups-end)
           (loop for start across groups-start
                 for end   across groups-end
                 do (add-tag visual-widget "group" :start (make-pos line start) :end (make-pos line end))))
         (update-highlights-single-line (scanner)
           (loop for line from 1 to (+ 1 visual-lines)
                 for line-text = (get-text-line visual-widget line)
                 do (ppcre:do-scans (match-start match-end groups-start groups-end scanner line-text)
                      (when match-start
                        (set-match-highlights-single line match-start match-end)
                        (set-groups-highlights-single line groups-start groups-end)))))

         (text-change (&optional (event nil))
           (declare (ignore event))

           (remove-tags visual-widget tags-highlights)
           (remove-tags regex-widget  tags-highlights)
           (setf visual-lines (lines visual-widget))
           (let ((regex-text (get-text regex-widget)))
             (handler-case (ppcre:create-scanner regex-text :extended-mode extended-mode :multi-line-mode multi-line-mode)
               (error (c)
                      (progn (add-tag regex-widget "error")
                             (format t "Error in regex: ~%~A" c)))
               (:no-error (v registers)
                (declare (ignore registers))
                (if multi-line-mode 
                    (update-highlights-multi-line  v)
                    (update-highlights-single-line v))))))
         (multi-line-mode-change (value) 
           (setf multi-line-mode (= value 1))
           (text-change))
         (extended-mode-change (value) 
           (setf extended-mode  (= value 1))
           (text-change))
         (main ()
           (initialize-size-pos)
           (set-tags visual-widget tags-highlights) ; set highlighting tags
           (set-tags regex-widget  tags-highlights) ; set highlighting tags
           ; to-do
           ; create separate function to create scanner only when <KeyRelease> occurs on regex-widget
           (ltk:bind regex-widget  "<KeyRelease>" #'text-change)
           (ltk:bind visual-widget "<KeyRelease>" #'text-change)
           (setf (ltk:command multi-line-mode-button) #'multi-line-mode-change)
           (setf (ltk:command extended-mode-button)   #'extended-mode-change)
           ))
        (main)))))

