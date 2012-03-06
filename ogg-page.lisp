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

(defclass ogg-stream (fundamental-binary-stream
                      trivial-gray-stream-mixin)
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
    (read-object page stream)
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
      (prog1
          (cond ((= bits-left 8)
                 (aref data position))
                (t
                 (logand (ash bits-left
                              (print (mask-field (byte (- 8 bits-left) 0) (aref data (1+ position)))))
                         (ldb (byte bits-left
                                    (- 8 bits-left))
                              (aref data position)))))
        (incf (ogg-page-position stream))))))

(defmethod stream-read-sequence ((stream ogg-stream) sequence start end &key)
  (loop for i from start below (or end (length sequence))
        do (setf (aref sequence i)
                 (read-byte stream))
        finally (return i)))

(defun read-bit (stream)
  (let ((position (ogg-page-position stream)))
    (when (= position (ogg-page-length stream))
      (setf position 0)
      (refill-stream stream))
    (let ((data (data (ogg-page stream)))
          (bits-left (bits-left stream)))
      (prog1
          (logbitp (- 8 bits-left) (aref data position))
        (cond ((= bits-left 1)
               (incf (ogg-page-position stream))
               (setf (bits-left stream) 8))
              (t
               (decf (bits-left stream))))))))

(defun read-n-bits (n stream)
  (let ((position (ogg-page-position stream)))
    (when (= position (ogg-page-length stream))
      (setf position 0)
      (refill-stream stream))
    (let ((data (data (ogg-page stream)))
          (bits-left (bits-left stream)))
      (prog1
          (logand (ash bits-left
                       (mask-field (byte (- n bits-left) 0) (aref data (1+ position))))
                  (if (> bits-left n)
                      0
                      (ldb (byte bits-left
                                 (- n bits-left))
                           (aref data position))))
        (cond ((= bits-left n)
               (incf (ogg-page-position stream))
               (setf (bits-left stream) 8))
              (t
               (decf (bits-left stream) n)))))))
