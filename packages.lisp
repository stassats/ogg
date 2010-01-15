;;; -*- Mode: Lisp -*-

(defpackage #:binary-data
  (:use #:cl)
  (:export #:define-binary-class
           #:define-tagged-binary-class
           #:define-binary-type
           #:read-value
           #:write-value
           #:read-object
           #:*in-progress-objects*
           #:parent-of-type
           #:current-binary-object
           #:+null+))

(defpackage #:ogg
  (:use #:cl #:binary-data
        #:trivial-gray-streams)
  (:export ))
