(defpackage :aoc2020.04
  (:use :aoc2020)
  (:export #:part-1
           #:part-2
           #:test))

(in-package :aoc2020.04)

(defun map-line-chunks (function &aux stack)
  "Read consecutive non-empty lines and call FUNCTION on their concatenation."
  (flet ((emit ()
           (when stack
             (let ((lines (nreverse (shiftf stack nil))))
               (funcall function (format nil "~{~a~^ ~}" lines))))))
    (do-input-lines (line 4 (emit))
      (if (string= line "")
          (emit)
          (push line stack)))))

(defpackage aoc2020.04.fields
  (:documentation "Expected symbols for fields in a record.")
  (:use)
  (:export #:byr
           #:iyr
           #:eyr
           #:hgt
           #:hcl
           #:ecl
           #:pid
           #:cid))

(defun field (name)
  (or (find-symbol (string-upcase name) :aoc2020.04.fields)
      (error "Unexpected field ~s" name)))

(defun map-records (function)
  "Call FUNCTION with a (FIELD . VALUE) association list for all records."
  (map-line-chunks
   (lambda (line)
     (funcall function
              (loop
                :for field :in (split #\space line :sharedp t)
                :for (name value) := (split #\: field :sharedp t)
                :collect (cons (field name) value))))))

(defun count-records-if (test &aux (counter 0))
  "Count records that match TEST"
  (map-records (lambda (u) (when (funcall test u) (incf counter))))
  counter)

(defun validate-all-fields-if (test record)
  "Check that for each expected FIELD, (FUNCALL TEST FIELD VALUE) is true.
   VALUE is the associated value for FIELD in RECORD, and may be NIL."
  (do-external-symbols (field :aoc2020.04.fields t)
    (unless (funcall test field (cdr (assoc field record)))
      (return nil))))

(defun solve-part (validation-test)
  "Count all records that match VALIDATION-TEST"
  (count-records-if
   (lambda (record)
     (validate-all-fields-if validation-test record))))

;; PART-1

(defun part-1 ()
  (solve-part
   (lambda (key value)
     (case key
       ;; cid is always valid, even if not present
       (aoc2020.04.fields:cid t)
       ;; other fields are mandatory
       (t value)))))

;; PART-2

(declaim (inline year<= valid-height-p))

(defun year<= (min string max)
  (and string
       (= 4 (length string))
       (<= min (parse-integer string) max)))

(defun valid-height-p (string)
  (when string
    (multiple-value-bind (height end) (parse-integer string :junk-allowed t)
      (when height
        (flet ((unitp (u) (string= string u :start1 end)))
          (cond
            ((unitp "in") (<=  59 height  76))
            ((unitp "cm") (<= 150 height 193))))))))

(defmacro regexp (v r)
  (check-type v symbol)
  `(and ,v (scan ,r ,v)))

;; I went into a bit of over-engineering rabbit hole

(defmacro define-validators ((name var) &body clauses)
  "Build an hash-table from fields to validation functions.
   (check exhaustiveness, mutliple entries for a field mean AND)"
  (check-type name symbol)
  (check-type var symbol)
  (with-gensyms (hash-table)
    (let (initforms)
      (flet ((add-initform (field expressions)
               (push `(setf (gethash ',field ,hash-table)
                            (compile nil
                                     (lambda (,var)
                                       (declare (ignorable ,var))
                                       (and ,@expressions))))
                     initforms)))
        (do-external-symbols (field :aoc2020.04.fields)
          (let ((clauses (remove field clauses :test-not #'eql :key #'first)))
            (if clauses
                (add-initform field (map-into clauses #'second clauses))
                (warn "No rule for field ~s" field)))))
      `(defvar ,name
         (let ((,hash-table (make-hash-table)))
           (prog1 ,hash-table
             ,@initforms))))))

(define-validators (*part-2-validators* v)
  (aoc2020.04.fields:byr (year<= 1920 v 2002))
  (aoc2020.04.fields:iyr (year<= 2010 v 2020))
  (aoc2020.04.fields:eyr (year<= 2020 v 2030))
  (aoc2020.04.fields:hgt (valid-height-p v))
  (aoc2020.04.fields:hcl (regexp v "^#[0-9a-f]{6}$"))
  (aoc2020.04.fields:ecl (regexp v "^(?:amb|blu|brn|gry|grn|hzl|oth)$"))
  (aoc2020.04.fields:pid (regexp v "^\\d{9}$"))
  (aoc2020.04.fields:cid t))

(defun part-2 (&aux (h *part-2-validators*))
  (solve-part  (lambda (k v) (funcall (gethash k h) v))))

(defun test ()
  (assert (= 196 (part-1)))
  (assert (= 114 (part-2))))
