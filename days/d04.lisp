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
  (:nicknames d04.f)
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

(defun validate-all-fields-if (test record)
  (do-external-symbols (field :aoc2020.04.fields t)
    (unless (funcall test field (cdr (assoc field record)))
      (return nil))))

(defun solve-part (validator)
  (count-records-if
   (lambda (record)
     (validate-all-fields-if validator record))))

(defun part-1 ()
  (solve-part
   (lambda (k v)
     (case k
       (aoc2020.04.fields:cid t)
       (t v)))))

(defun yearp (s min max)
  (when s
    (and (= 4 (length s)) (<= min (parse-integer s) max))))

(defun heightp (s)
  (when s
    (multiple-value-bind (height end) (parse-integer s :junk-allowed t)
      (when height
        (let ((unit (subseq s end)))
          (cond
            ((string= unit "in") (<= 59 height 76))
            ((string= unit "cm") (<= 150 height 193))))))))

(defun eclp (v)
  (when v
    (scan '(:sequence
            :start-anchor
            (:alternation
             "amb" "blu" "brn"
             "gry" "grn" "hzl" "oth")
            :end-anchor)
          v)))

(defun pidp (v)
  (when v
    (scan '(:sequence :start-anchor
            (:greedy-repetition 9 9 :digit-class)
            :end-anchor)
          v)))

(defun part-2 ()
  (solve-part
   (lambda (k v)
     (case k
       (d04.f:byr (yearp v 1920 2002))
       (d04.f:iyr (yearp v 2010 2020))
       (d04.f:eyr (yearp v 2020 2030))
       (d04.f:hgt (heightp v))
       (d04.f:hcl (and v (scan "^#[0-9a-f]{6,6}$" v)))
       (d04.f:ecl (eclp v))
       (d04.f:pid (pidp v))
       (d04.f:cid t)))))

(defun test ()
  (assert (= 196 (part-1)))
  (assert (= 114 (part-2))))
