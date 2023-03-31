(defpackage #:sub-reader/parser
  (:use #:cl)
  (:export #:load-subtitles
           #:sub-start
           #:sub-end
           #:sub-text
           #:make-sub))

(in-package #:sub-reader/parser)

(defun spec-sec (hour min sec ms)
  (+ (/ ms 1000)
     sec
     (* 60 (+ min (* 60 hour)))))

(defun timestamp-p (line)
  (multiple-value-bind (spec spec-p)
      (snf:scanf "%d:%d:%d,%d --> %d:%d:%d,%d" line)
    (if (and spec spec-p)
        (destructuring-bind (h1 m1 s1 ms1 h2 m2 s2 ms2)
            spec
          (list (spec-sec h1 m1 s1 ms1)
                (spec-sec h2 m2 s2 ms2))))))

(defstruct (sub (:constructor make-sub
                    (interval text
                     &aux (start (elt interval 0))
                          (end (elt interval 1)))))
  start end text)

(defun load-subtitles (filename &rest open-args &key &allow-other-keys)
  (let ((lines (apply #'uiop:read-file-lines filename open-args)))
    (labels ((read-one ()
               (alexandria:when-let ((interval (timestamp-p (pop lines))))
                 (make-sub interval
                           (format nil "~{~a~^~%~}"
                                   (loop for line = (pop lines)
                                         until (string= line "")
                                         until (null line)
                                         collect line))))))
      (loop while lines
            for sub = (read-one)
            when sub collect sub into subs
            finally (return (sort (coerce subs 'vector) #'< :key #'sub-start))))))
