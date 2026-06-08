
(asdf:defsystem #:regex-visualizer
  :author "Maximilian Ballard"
  :license  "GPLv3"
  :version "0.0.9"
  :serial t
  :depends-on (
               :maximilian-utils ; general utilities
               :cl-ppcre         ; regex
               :ltk              ; gui
               )
  :components ((:module "src"
                :components
                ((:file "package" )
                 (:file "utilities")
                 (:file "main")
                 ))
               (:static-file "LICENSE" :pathname #P"LICENSE")
               (:static-file "README.md" :pathname #P"README.md")
               )
  :description "regex visualizer using cl-ppcre and tcl"
  :long-description #.(uiop:read-file-string  (merge-pathnames "README.md" *load-pathname*))
  :build-operation program-op
  :build-pathname "regex-visualizer"
  :entry-point "regex-visualizer::draw-main"
  
  )

