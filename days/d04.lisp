(defpackage :aoc2020.04
  (:use :aoc2020))

(in-package :aoc2020.04)

(defun map-chunks (fn &aux stack)
  (flet ((emit ()
           (when stack
             (let ((lines (nreverse (shiftf stack nil))))
               (funcall fn (format nil "~{~a~^ ~}" lines))))))
    (do-input-lines (line 4 (emit))
      (if (string= line "")
          (emit)
          (push line stack)))))

(defpackage aoc2020.04.fields
  (:use)
  (:export #:byr
           #:iyr
           #:eyr
           #:hgt
           #:hcl
           #:ecl
           #:pid
           #:cid))

(defun map-records (function)
  (map-chunks
   (lambda (line)
     (funcall
      function
      (loop
         :for field :in (split #\space line :sharedp t)
         :for (name value) := (split #\: field :sharedp t)
         :collect (cons (find-symbol (string-upcase name)
                                     :aoc2020.04.fields)
                        value))))))

(defun count-records-if (test &aux (counter 0))
  (map-records (lambda (record)
                 (when (funcall test record)
                   (incf counter))))
  counter)

(defun part-1 ()
  (count-records-if (lambda (record)
                      (case (length record)
                        (8 t)
                        (7 (not
                            (assoc 'aoc2020.04.fields:cid record)))))))

(defun test ()
  (assert (= 196 (part-1))))
