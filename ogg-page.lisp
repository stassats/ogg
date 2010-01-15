;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:ogg)

(define-binary-type ascii-string (length)
  (:reader (in)
           (let ((string (make-string length)))
             (loop for i below length
                   do (setf (char string i)
                            (code-char (read-byte in))))
             string))
  (:writer (out value)))

(define-binary-type integer (bytes (bits-per-byte 8))
  (:reader (in)
    (loop with value = 0
          for lsb to (* bits-per-byte (1- bytes)) by bits-per-byte do
          (setf (ldb (byte bits-per-byte lsb) value) (read-byte in))
       finally (return value)))
  (:writer (out value)
           (loop for lsb to (* bits-per-byte (1- bytes)) by bits-per-byte
                 do (write-byte (ldb (byte bits-per-byte lsb) value) out))))

(define-binary-type u1 () (integer :bytes 1))
(define-binary-type u4 () (integer :bytes 4))

;;;

(define-binary-type vector (length)
  (:reader (in)
           (let ((vector (make-array length :element-type '(unsigned-byte 8))))
             (read-sequence vector in)
             vector))
  (:writer (out value)))

(define-binary-type header-type-flag ()
  (:reader (in)
           (let ((byte (read-byte in)))
             (values (logbitp 0 byte)
                     (logbitp 1 byte)
                     (logbitp 3 byte))))
  (:writer (out value)))

(define-binary-type data-size (length)
  (:reader (in)
           (loop repeat length
                 sum (read-byte in)))
  (:writer (out value)))

(define-binary-class ogg-page ()
  ((magick   (ascii-string :length 4))
   (version  u1)
   (type-flag  header-type-flag)
   (granule-position (vector :length 8))
   (bitstream-serial-number u4)
   (page-sequence-number u4)
   (crc u4)
   (number-page-segments u1)
   (data-size (data-size :length number-page-segments))
   (data (vector :length data-size))))

(defun parse-page (stream)
  (read-value 'ogg-page stream))

(defun read-file (file)
  (with-open-file (stream file :element-type 'unsigned-byte)
    (parse-page stream)))

(defmacro with-ogg-stream ((stream file) &body body)
  (let ((file-stream (gensym)))
    `(with-open-file (,file-stream ,file :element-type 'unsigned-byte)
       (let ((,stream (make-instance 'ogg-stream :stream ,file-stream)))
         ,@body))))

(defclass ogg-stream (fundamental-binary-stream
                      trivial-gray-stream-mixin)
  ((stream :initarg :stream
           :reader ogg-stream)
   (page :initform (make-instance 'ogg-page)
         :reader ogg-page)
   (position :initform 0 :accessor ogg-page-position)
   (length :initform 0 :accessor ogg-page-length)))

(defun refill-stream (ogg-stream)
  (with-slots (stream page position length) ogg-stream
    (read-object page stream)
    (setf position 0
          length (data-size page)))
  ogg-stream)

(defmethod stream-read-byte ((stream ogg-stream))
  (when (= (ogg-page-position stream)
           (ogg-page-length stream))
    (refill-stream stream))
  (prog1 (aref (data (ogg-page stream))
               (ogg-page-position stream))
    (incf (ogg-page-position stream))))

(defmethod stream-read-sequence ((stream ogg-stream) sequence start end &key)
  (loop for i from (or start 0) below (max (length sequence)
                                           (or end 0))
        do (setf (aref sequence i)
                 (read-byte stream))
        finally (return i)))
