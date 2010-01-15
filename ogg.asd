;;; -*- Mode: Lisp -*-

(asdf:defsystem #:ogg
  :serial t
  :depends-on (trivial-gray-streams)
  :components ((:file "packages")
               (:file "binary-data")
               (:file "ogg-page")
               (:file "ogg-vorbis")))
