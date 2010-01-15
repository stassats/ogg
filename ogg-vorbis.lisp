;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:ogg)

(define-binary-type utf8-string (length)
  (:reader (in)
           (babel:octets-to-string
            (read-value 'vector in :length length)
            :encoding :utf-8))
  (:writer (out value)))

(define-tagged-binary-class vorbis ()
  ((packet-type u1)
   (magick   (ascii-string :length 6)))
  (:dispatch
   (ecase packet-type
     (1 'vorbis-id-header)
     (3 'vorbis-comment-header))))

(define-binary-class vorbis-id-header (vorbis)
  ((version u4)
   (audio-channels u1)
   (audio-sample-rate u4)
   (bitrate-maximum u4)
   (bitrate-nominal u4)
   (bitrate-minimum u4)
   (block-size u1)
   (framing-flag u1)))

(define-binary-class comment ()
  ((comment-length u4)
   (comment (utf8-string :length comment-length))))

(define-binary-type comments (length)
  (:reader (in)
           (loop repeat length
                 nconc (parse-comment-string
                        (comment (read-value 'comment in)))))
  (:writer (out value)))

(define-binary-class vorbis-comment-header (vorbis)
  ((vendor-length u4)
   (vendor-string (utf8-string :length vendor-length))
   (comments-length u4)
   (comments (comments :length comments-length))))

(defun parse-comment-string (string)
  (let ((=-position (position #\= string)))
    (assert (numberp =-position))
    (list (intern (nstring-upcase (subseq string 0 =-position)) :keyword)
          (subseq string (1+ =-position)))))
