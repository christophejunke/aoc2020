(defpackage :aoc2020.fetch
  (:use :cl :drakma)
  (:export #:fetch-input))

(in-package :aoc2020.fetch)

(defun input-url (index)
  (format nil "https://adventofcode.com/2020/day/~d/input" index))

;; Once logged with the browser, it is possible to get the
;; necessary cookies (e.g. "Open as cURL", Network tab in
;; Chrome). My cookie file looks like this:
;;
;; _ga=....; _gid=....; _gat=...; session=...

(defvar *cookie-file*
  (merge-pathnames ".aoc-cookies" (user-homedir-pathname)))

(defvar *headers*
  `(("authority" . "adventofcode.com")
    ("cache-control" . "max-age=0")
    ("dnt" . "1")
    ("upgrade-insecure-requests" . "1")
    ("sec-fetch-site" . "same-origin")
    ("sec-fetch-mode" . "navigate")
    ("sec-fetch-user" . "?1")
    ("sec-fetch-dest" . "document")
    ("cookie" . ,(with-open-file (i *cookie-file*) (read-line i)))))

(defun aoc-input-stream (day-number)
  (http-request (input-url day-number)
                :additional-headers *headers*
                :want-stream t))

(defvar *input-base*
  (merge-pathnames "inputs/*.txt"
                   (asdf:system-source-directory "aoc2020")))

(defun input-pathname (in)
  (typecase in
    (number (input-pathname (format nil "~2,'0d" in)))
    (string (input-pathname (make-pathname :name in)))
    (pathname (merge-pathnames in *input-base*))))

;;
;; Use responsibly, do not call too often
;;

(defparameter *next-fetch-time* nil)

(defparameter *fetch-limit*
  (local-time-duration:duration :minute 1))

(defun allow-fetch-p (&aux (now (local-time:now)))
  (or (null *next-fetch-time*)
      (and (local-time:timestamp>= now *next-fetch-time*)
           (setf *next-fetch-time*
                 (local-time-duration:timestamp-duration+ now
                                                          *fetch-limit*)))))

(defun fetch-input (day &optional (warnp t))
  (let ((day-pathname (input-pathname day)))
    (prog1 day-pathname
      (if (probe-file day-pathname)
          (when warnp
            (warn "File already exists: ~a" day-pathname))
          (if (allow-fetch-p)
              (with-open-file (out day-pathname :direction :output)
                (with-open-stream (in (aoc-input-stream day))
                  (uiop:copy-stream-to-stream in out)))
              (error "Next fetch allowed: ~a" *next-fetch-time*))))))
