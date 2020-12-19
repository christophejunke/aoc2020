(defpackage :aoc2020.19
  (:use :aoc2020))

(in-package :aoc2020.19)

(defun scanner ()
  (load-time-value
   (create-scanner
    '(:sequence #\" (:register (:regex "\\w+")) #\"))))

(defun parse-rule (line)
  (destructuring-bind (line body) (split ": " line)
    (cons (parse-integer line)
          (or (register-groups-bind (s) ((scanner) body) (and s (char s 0)))
              (loop for part in (split '(:group " | ") body)
                    collect (mapcar #'parse-integer (split #\space part)))))))

(defun make-input (&key rules entries)
  (values rules entries))

(defun input (name)
  (with-input (in name)
    (make-input
     :rules (let ((rules (list)))
              (do-input-lines (line in (error "no entry"))
                (when (= (length line) 0)
                  (return rules))
                (push (parse-rule line) rules)))
     :entries (map-input in))))
