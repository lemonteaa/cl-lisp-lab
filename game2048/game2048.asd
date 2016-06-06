;;;; game2048.asd

(asdf:defsystem #:game2048
  :serial t
  :description "A Lisp version for the famous game 2048"
  :author "lemontea <lemontea.Tom@gmail.com>"
  :license "Specify license here"
  :components ((:file "package")
               (:file "game2048")))

