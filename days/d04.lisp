(defpackage :aoc2020.04
  (:use :aoc2020)
  (:export #:part-1
           #:part-2
           #:test))

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

(defun map-records (fn)
  (map-chunks
   (lambda (line)
     (funcall fn
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

(defun validate-all-fields-if (test record)
  (do-external-symbols (field :aoc2020.04.fields t)
    (unless (funcall test field (cdr (assoc field record)))
      (return nil))))

(defun solve-part (validation-test)
  (count-records-if
   (lambda (record)
     (validate-all-fields-if validation-test record))))

(defun part-1 ()
  (solve-part
   (lambda (key value)
     (case key
       ;; cid is always valid, even if not present
       (aoc2020.04.fields:cid t)
       ;; other fields are mandatory
       (t value)))))

(defun yearp (string min max)
  (and string
       (= 4 (length string))
       (<= min (parse-integer string) max)))

(defun heightp (string)
  (when string
    (multiple-value-bind (height end) (parse-integer string :junk-allowed t)
      (when height
        (flet ((unitp (u) (string= string u :start1 end)))
          (cond
            ((unitp "in") (<=  59 height  76))
            ((unitp "cm") (<= 150 height 193))))))))

(defun part-2 ()
  (solve-part
   (lambda (k v)
     (macrolet ((rgxp (r) `(and v (scan ,r v)))
                (year (min max) `(yearp v ,min ,max)))
       (case k
         (aoc2020.04.fields:byr (year 1920 2002))
         (aoc2020.04.fields:iyr (year 2010 2020))
         (aoc2020.04.fields:eyr (year 2020 2030))
         (aoc2020.04.fields:hgt (heightp v))
         (aoc2020.04.fields:hcl (rgxp "^#[0-9a-f]{6}$"))
         (aoc2020.04.fields:ecl (rgxp "^(?:amb|blu|brn|gry|grn|hzl|oth)$"))
         (aoc2020.04.fields:pid (rgxp "^\\d{9}$"))
         (aoc2020.04.fields:cid t))))))

(defun test ()
  (assert (= 196 (part-1)))
  (assert (= 114 (part-2))))
