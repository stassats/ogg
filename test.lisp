(in-package :ogg)

(defun test ()
  (with-ogg-stream (stream "~/01_expander.ogg")
    (list (read-value 'vorbis stream)
          (read-value 'vorbis stream)
          (read-value 'vorbis stream))))
