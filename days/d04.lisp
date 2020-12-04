(aoc2020.utils:defpackage/enum :aoc2020.04.fields
  "Expected symbols for fields in a record."
  (#:BYR "Birth Year")
  (#:IYR "Issue Year")
  (#:EYR "Expiration Year")
  (#:HGT "Height")
  (#:HCL "Hair Color")
  (#:ECL "Eye Color")
  (#:PID "Passport ID")
  (#:CID "Country ID"))

(defpackage :aoc2020.04
  (:use :aoc2020 :aoc2020.04.fields)
  (:export #:solve
           #:test))

(in-package :aoc2020.04)

(define-constant +fields-count+
    (length (aoc2020.utils:external-symbols
             :aoc2020.04.fields)))

(defun field (s i)
  "Find field symbol from NAME."
  (case (char s i)
    (#\b 'byr)
    (#\c 'cid)
    (#\i 'iyr)
    (#\p 'pid)
    (#\e (case (char s (1+ i))
           (#\y 'eyr)
           (t 'ecl)))
    (#\h (case (char s (1+ i))
           (#\c 'hcl)
           (t 'hgt)))))

(defun map-line-chunks (function &aux stack)
  "Read consecutive non-empty lines and call FUNCTION on their concatenation."
  (flet ((emit ()
           (when stack
             (let ((lines (nreverse (shiftf stack nil))))
               (funcall function lines)))))
    (do-input-lines (line 4 (emit))
      (if (= 0 (length line))
          (emit)
          (push line stack)))))

(defun map-fields (function line)
  (declare (type (simple-array character (*)) line)
           (type function function)
           (optimize (speed 3)))
  (loop
    :for start = 0 :then (1+ find)
    :for find = (position #\space line :start start)
    :for end = (or find (length line))
    :for colon = (position #\: line :start start)
    :do
       (assert (<= start colon end) ())
       (funcall function
                (field line start)
                (subseq line (1+ colon) end))
    :while find))

(defun map-records (function)
  "Call FUNCTION with a (FIELD . VALUE) association list for all records."
  (let ((record (make-array +fields-count+
                            :element-type '(or null string)
                            :initial-element nil)))
    (declare (dynamic-extent record))
    (map-line-chunks
     (lambda (lines)
       (dolist (line lines)
         (map-fields (lambda (field value)
                       (setf (aref record (symbol-value field)) value))
                     line))
       (funcall function record)
       (fill record nil)))))

(defun validate-all-fields-if (test record)
  "Check that for each expected FIELD, (FUNCALL TEST FIELD VALUE) is true.
   VALUE is the associated value for FIELD in RECORD, and may be NIL."
  (do-external-symbols (field :aoc2020.04.fields t)
    (let ((index (symbol-value field)))
      (unless (funcall test field (aref record index))
        (return nil)))))

(declaim (inline year<= valid-height-p))

(defmacro year<= (min string max)
  (check-type min number)
  (check-type max number)
  (check-type string symbol)
  `(and ,string
        ;; (= 4 (length ,string))
        (string<= ,(princ-to-string min) ,string)
        (string<= ,string ,(princ-to-string max))))

(defun valid-height-p (string)
  (when string
    (if (char= #\m (char string (1- (length string))))
        (and (string<= "150" string :end2 3)
             (string<= string "193" :end1 3))
        (and (string<= "59" string :end2 2)
             (string<= string "76" :end1 2)))))

(defmacro regexp (v r)
  (check-type v symbol)
  `(and ,v (scan ,r ,v)))

(defmacro define-validator (fun-name (val-name) &body body)
  "Define FUN-NAME as a validator predicate of two parameters FIELD and VALUE.

   BODY is a (FIELD EXPRESSION) form. Symbol VAL-NAME will be bound in
   EXPRESSION to the associated value for the FIELD.

   Duplicate clauses for the same FIELD are treated as an AND, their
   associated expressions are evaluated in the some order as they are
   declared.

   The validator should be exhaustive, meaning that all expected fields should
   have at least one expression. Otherwise, a warning is emitted for each
   missing field."
  (check-type fun-name symbol)
  (check-type val-name symbol)
  (with-gensyms (key jump-table)
    (let (initforms)
      (flet ((add-initform (field expressions)
               (push `(setf (aref ,jump-table ,(symbol-value field))
                            (compile nil
                                     (lambda (,val-name)
                                       (declare (ignorable ,val-name))
                                       (and ,@expressions))))
                     initforms)))
        (do-external-symbols (field :aoc2020.04.fields)
          (if-let (clauses (remove field body :test-not #'string= :key #'car))
            (add-initform field (map-into clauses #'second clauses))
            (warn "No rule for field ~s" field))))
      `(let ((,jump-table (make-array +fields-count+
                           :element-type 'function
                           :initial-element #'values)))
         ,@initforms
         (defun ,fun-name (,key ,val-name)
           (funcall (aref ,jump-table (symbol-value ,key)) ,val-name))))))

(define-validator part-1/validp (v)
  ;; all fields are mandatory (v is not null), except CID
  (BYR v)
  (IYR v)
  (EYR v)
  (HGT v)
  (HCL v)
  (ECL v)
  (PID v)
  (CID t))

(define-validator part-2/validp (v)
  (BYR (year<= 1920 v 2002))
  (IYR (year<= 2010 v 2020))
  (EYR (year<= 2020 v 2030))
  (HGT (valid-height-p v))
  (HCL (regexp v "^#[0-9a-f]{6}$"))
  (ECL (regexp v "^(?:amb|blu|brn|gry|grn|hzl|oth)$"))
  (PID (regexp v "^\\d{9}$"))
  (CID t))

(defun solve (&aux (p1 0) (p2 0))
  (map-records
   (lambda (record)
     (when (validate-all-fields-if #'part-1/validp record) (incf p1))
     (when (validate-all-fields-if #'part-2/validp record) (incf p2))))
  (values p1 p2))

(defun test ()
  (multiple-value-bind (p1 p2) (solve)
    (assert (= p1 196))
    (assert (= p2 114))))

