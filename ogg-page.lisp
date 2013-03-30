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

(define-binary-type integer (bytes)
  (:reader (in)
           (loop with value = 0
                 for lsb to (* 8 (1- bytes)) by 8 do
                 (setf (ldb (byte 8 lsb) value) (read-byte in))
                 finally (return value)))
  (:writer (out value)
           (loop for lsb to (* 8 (1- bytes)) by 8
                 do (write-byte (ldb (byte 8 lsb) value) out))))

(define-binary-type u1 () (integer :bytes 1))
(define-binary-type u2 () (integer :bytes 2))
(define-binary-type u3 () (integer :bytes 3))
(define-binary-type u4 () (integer :bytes 4))

(define-binary-type 1-bit ()
  (:reader (in)
           (read-bit in))
  (:writer (out value)))

(define-binary-type n-bits (n)
  (:reader (in)
           (read-n-bits n in))
  (:writer (out value)))

;;;

(define-binary-type vector (length)
  (:reader (in)
           (let ((vector (make-array length :element-type '(unsigned-byte 8))))
             (read-sequence vector in)
             vector))
  (:writer (out value)
           (write-sequence value out)))

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
  ((magick (ascii-string :length 4))
   (version u1)
   (type-flag header-type-flag)
   (granule-position (vector :length 8))
   (bitstream-serial-number u4)
   (page-sequence-number u4)
   (crc u4)
   (number-page-segments u1)
   (data-size (data-size :length number-page-segments))
   (data (vector :length data-size))))

(defclass ogg-stream (fundamental-binary-input-stream)
  ((stream :initarg :stream
           :reader ogg-stream)
   (page :initform (make-instance 'ogg-page)
         :reader ogg-page)
   (position :initform 0
             :accessor ogg-page-position)
   (length :initform 0
           :accessor ogg-page-length)
   (bits-left :initarg :bits-left
              :initform 8
              :accessor bits-left)))

(defmacro with-ogg-stream ((stream file &key) &body body)
  (let ((file-stream (gensym)))
    `(with-open-file (,file-stream ,file :element-type '(unsigned-byte 8))
       (let ((,stream (make-instance 'ogg-stream :stream ,file-stream)))
         ,@body))))

(defun refill-stream (ogg-stream)
  (with-slots (stream page position length) ogg-stream
    (binary-data::read-object page stream)
    (setf position 0
          length (data-size page)
          (bits-left ogg-stream) 8))
  ogg-stream)

(defmethod stream-read-byte ((stream ogg-stream))
  (let ((position (ogg-page-position stream)))
    (when (= position (ogg-page-length stream))
      (setf position 0)
      (refill-stream stream))
    (let ((data (data (ogg-page stream)))
          (bits-left (bits-left stream)))
      (cond ((= bits-left 8)
             (prog1
                 (aref data position)
               (incf (ogg-page-position stream))))
            (t
             (read-n-bits 8 stream))))))

(defmethod stream-read-sequence ((stream ogg-stream) sequence start end &key)
  (loop for i from start below (or end (length sequence))
        do (setf (aref sequence i)
                 (read-byte stream))
        finally (return i)))

(defun read-bit (stream)
  (plusp (read-n-bits 1 stream)))

(defun read-n-bits (n stream)
  (cond ((> n 8)
         (multiple-value-bind (quot rem) (floor n 8)
           (logior (read-n-bits rem stream))))
        (t
         (let ((position (ogg-page-position stream)))
           (when (= position (ogg-page-length stream))
             (setf position 0)
             (refill-stream stream))
           (let ((data (data (ogg-page stream)))
                 (bits-left (bits-left stream)))
             (prog1
                 (if (> n bits-left)
                     (logior (ldb (byte (min n bits-left) (- 8 bits-left))
                                  (aref data position))
                             (ash (ldb (byte (- n bits-left) 0)
                                       (aref data (1+ position)))
                                  bits-left))
                     (ldb (byte (min n bits-left) (- 8 bits-left))
                          (aref data position)))
               (cond ((> n bits-left)
                      (incf (ogg-page-position stream))
                      (setf (bits-left stream)
                            (- 8 (- n bits-left))))
                     (t
                      (decf (bits-left stream) n)))))))))
