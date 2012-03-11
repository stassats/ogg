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
   (codebooks (n-things :thing 'codebook
                        :length (1+ vorbis-codebook-count)))))

(define-binary-class codebook ()
  ((sync-pattern u3)
   (dimensions u2)
   (codebook-length u3)
   (ordered 1-bit)
   (codebooks (codebook-entries :length codebook-length
                                :ordered ordered))
   (lookup-type (n-bits :n 4))
   (lookup (lookup :type lookup-type
                   :length codebook-length
                   :dimensions dimensions))))

(defun read-unordered-codebook-entries (length stream)
  (let ((result (make-array length)))
    (loop for i below length
          with sparse = (read-bit stream)
          do (setf (aref result i)
                   (if (and sparse
                            (not (read-bit stream)))
                       :unused
                       (1+ (read-n-bits 5 stream)))))
    result))

(defun read-ordered-codebook-entries (length stream)
  (let ((result (make-array length)))
    (loop for current-length from (1+ (read-n-bits 5 stream))
          for current-entry = 0 then (+ current-entry number)
          for number = (cond ((> current-entry length)
                              (error "Can't happen"))
                             ((= current-entry length)
                              (return))
                             (t
                              (read-n-bits (integer-length (- length current-entry)) stream)))
          do
          (loop for i from current-entry below (+ current-entry number)
                do (setf (aref result i) current-length)))
    result))

(define-binary-type codebook-entries (length ordered)
  (:reader (in)
           (if ordered
               (read-ordered-codebook-entries length in)
               (read-unordered-codebook-entries length in)))
  (:writer (out value)))

(defun lookup1-values (length dimensions)
  (let ((r (floor (exp (/ (log length) dimensions)))))
    (if (>= length (expt (1+ r) dimensions))
        (1+ r)
        r)))

(defun read-lookup-values (type length dimensions stream)
  (let* ((min (read-value 'u4 stream))
         (delta (read-value 'u4 stream))
         (bits (1+ (read-n-bits 4 stream)))
         (sequencep (read-bit stream))
         (size (if (= type 1)
                   (lookup1-values length dimensions)
                   (* length dimensions)))
         (result (make-array size)))
    (loop for i below size
          do (setf (aref result i)
                   (read-n-bits bits stream)))
    result))

(define-binary-type lookup (type length dimensions)
  (:reader (in)
           (ecase type
             (0)
             ((1 2)
              (read-lookup-values type length dimensions in))))
  (:writer (out value)))


