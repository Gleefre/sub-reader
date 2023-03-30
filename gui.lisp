(defpackage #:sub-reader/gui
  (:use #:cl #:sub-reader/parser)
  (:local-nicknames (#:sr/p #:sub-reader/parser))
  (:import-from #:sketch
                #:defsketch
                #:+black+
                #:gray
                #:text
                #:with-font #:make-font)
  (:export #:start))

(in-package #:sub-reader/gui)

(defun in-time (time sub &optional (lousy nil))
  (and (<= (sr/p:sub-start sub) time)
       (or lousy (<= time (sr/p:sub-end sub)))))

(defun match-time (time subs)
  (loop for i from (1- (length subs)) downto 0
        for sub = (aref subs i)
        when (in-time time sub t)
        do (return i)))

(defvar *subtitles*
  (vector (make-sub '(0 60)
"N - next  /  P - previous  /  R - reset
<space> - pause/run
<escape> - exit
L - load subtitle file (.srt)")))

(defsketch subtitle-window ((subs *subtitles*)
                            (sub-index 0)
                            (clock (clock:make-clock))
                            (show-late nil))
  ;; update current index
  (unless (clock:paused clock)
    (setf sub-index
          (or (match-time (clock:time clock) subs)
              sub-index)))
  ;; show if time matches
  (when (and (<= 0 sub-index (1- (length subs)))
             (or show-late (in-time (clock:time clock) (aref subs sub-index))))
    (with-font (make-font :align :center
                          :size 50
                          :color (if (in-time (clock:time clock) (aref subs sub-index))
                                     +black+
                                     (gray 0.2)))
      (text (sub-text (aref subs sub-index)) (/ sketch:width 2) 40)))
  (with-font (make-font :align :left :size 20)
    (text (format nil "~3,'_d / ~3,'_d" (1+ sub-index) (length subs)) 0 0)
    (text (format nil "~a" clock) 0 20)))

(defmethod kit.sdl2:keyboard-event ((app subtitle-window) state ts rep? keysym)
  (when (eq state :keydown)
    (with-slots (sub-index subs clock show-late) app
      (case (sdl2:scancode keysym)
        ((:scancode-n)
         (if (clock:paused clock)
             (when (< sub-index (1- (length subs)))
               (incf sub-index)
               (setf (clock:time clock)
                     (sub-start (aref subs sub-index))))
             (clock:shift clock 1/4)))
        ((:scancode-p)
         (if (clock:paused clock)
             (when (> sub-index 0)
               (decf sub-index)
               (setf (clock:time clock)
                     (sub-start (aref subs sub-index))))
             (clock:shift clock -1/4)))
        ((:scancode-space)
         (clock:toggle clock))
        ((:scancode-u :scancode-r)
         (setf subs *subtitles*
               sub-index 0)
         (clock:reset clock t)
         (when (plusp (length subs))
           (setf (clock:time clock)
                 (sub-start (aref subs 0)))))
        ((:scancode-h)
         (setf show-late (not show-late)))
        ((:scancode-l)
         (multiple-value-bind (file file-p)
             (org.shirakumo.file-select:existing)
           (when file-p
             (setf *subtitles* (load-subtitles file))
             (setf subs *subtitles*
                   sub-index 0)
             (clock:reset clock t)
             (when (plusp (length subs))
               (setf (clock:time clock)
                     (sub-start (aref subs 0)))))))))))

(sketch::define-start-function start
    subtitle-window
    (:resizable t :flags '(:always-on-top)
     :width 1000 :height 400))
