(defpackage :aoc2020.fetch
  (:use :cl :drakma)
  (:export #:fetch-for-day))

(in-package :aoc2020.fetch)

(defun input-url (index)
  (format nil "https://adventofcode.com/2020/day/~d/input" index))

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
    ("cookie" . ,(with-open-file (i *cookie-file*)
                   (read-line i)))))

(defun aoc-input-stream (day-number)
  (http-request (input-url day-number)
                :additional-headers *headers*
                :want-stream t))

(defun input-pathname (day)
  (merge-pathnames (make-pathname :name (format nil "~2,'0d" day)
                                  :type "txt"
                                  :directory '(:relative "inputs"))
                   (asdf:system-source-directory :aoc2020)))

;; do not call too often
(defun fetch-for-day (day)
  (let ((day-pathname (input-pathname day)))
    (prog1 day-pathname
      (if (probe-file day-pathname)
          (warn "File already exists")
          (with-open-file (out day-pathname :direction :output)
            (with-open-stream (in (aoc-input-stream day))
              (uiop:copy-stream-to-stream in out)))))))
