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
   (magick (ascii-string :length 6)))
  (:dispatch
   (ecase packet-type
     (1 'vorbis-id-header)
     (3 'vorbis-comment-header)
     (5 'vorbis-setup-header))))

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

(defun parse-comment-string (string)
  (let ((=-position (position #\= string)))
    (assert (numberp =-position))
    (list (intern (nstring-upcase (subseq string 0 =-position)) :keyword)
          (subseq string (1+ =-position)))))

(defun format-comments (comments)
  (loop for (key value) on comments by #'cddr
        collect (format nil "~:@(~a~)=~a" key value)))

(define-binary-type comments (length)
  (:reader (in)
           (loop repeat length
                 nconc (parse-comment-string
                        (comment (read-value 'comment in)))))
  (:writer (out value)))

(define-binary-type n-things (length thing)
  (:reader (in)
           (loop repeat length
                 collect (read-value thing in)))
  (:writer (out value)))

(define-binary-class vorbis-comment-header (vorbis)
  ((vendor-length u4)
   (vendor-string (utf8-string :length vendor-length))
   (comments-length u4)
   (comments (comments :length comments-length))
   (framing-flag u1)))

(define-binary-class vorbis-setup-header (vorbis)
  ((vorbis-codebook-count u1)
   (codebooks (n-things :thing 'codebook :length 2;; vorbis-codebook-count
                        ))))

(define-binary-class codebook ()
  ((sync-pattern u3)
   (codebook-dimensions u2)
   (n-codebook-entries u3)
   (ordered 1-bit)
   (code-books (codebook-entries :length n-codebook-entries
                                 :ordered ordered))
   (codebook-lookup-type (n-bits :n 5))))

(defun read-unordered-codebook-entries (length stream)
  (loop repeat length
        for sparse = (read-bit stream)
        collect (if (and sparse
                         (not (read-bit stream)))
                    :unused
                    (1+ (read-n-bits 5 stream)))))

(define-binary-type codebook-entries (length ordered)
  (:reader (in)
           (if ordered
               (error "doesn't supported unordered codebook entries")
               (read-unordered-codebook-entries length in)))
  (:writer (out value)))


