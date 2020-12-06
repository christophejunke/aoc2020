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
  (:export #:map-line-chunks
           #:solve
           #:test))

(in-package :aoc2020.04)

(defun field (name)
  "Find field symbol from NAME."
  (or (find-symbol (string-upcase name) :aoc2020.04.fields)
      (error "Unexpected field ~s" name)))

(defun map-line-chunks (in function &aux stack)
  "Read consecutive non-empty lines and call FUNCTION on their concatenation."
  (flet ((emit ()
           (when stack
             (let ((lines (nreverse (shiftf stack nil))))
               (funcall function lines)))))
    (do-input-lines (line in (emit))
      (if (string= line "")
          (emit)
          (push line stack)))))

;; /!\ returning an ALIST without doing further checks means we trust the
;; input to not have duplicate fields. We could also optimize so that it does
;; not search this list for each field.

(defun map-records (function)
  "Call FUNCTION with a (FIELD . VALUE) association list for all records."
  (map-line-chunks 04
                   (lambda (lines)
                     (let ((line (format nil "~{~a~^ ~}" lines)))
                       (funcall function
                                (loop
                                  :for field :in (split #\space line :sharedp t)
                                  :for (name value) := (split #\: field :sharedp t)
                                  :collect (cons (field name) value)))))))

(defun validate-all-fields-if (test record)
  "Check that for each expected FIELD, (FUNCALL TEST FIELD VALUE) is true.
   VALUE is the associated value for FIELD in RECORD, and may be NIL."
  (do-external-symbols (field :aoc2020.04.fields t)
    (unless (funcall test field (cdr (assoc field record)))
      (return nil))))

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
  (with-gensyms (key hash-table)
    (let (initforms)
      (flet ((add-initform (field expressions)
               (push `(setf (gethash ',field ,hash-table)
                            (compile nil
                                     (lambda (,val-name)
                                       (declare (ignorable ,val-name))
                                       (and ,@expressions))))
                     initforms)))
        (do-external-symbols (field :aoc2020.04.fields)
          (if-let (clauses (remove field body :test-not #'string= :key #'car))
            (add-initform field (map-into clauses #'second clauses))
            (warn "No rule for field ~s" field))))
      `(let ((,hash-table (make-hash-table)))
         ,@initforms
         (defun ,fun-name (,key ,val-name)
           (funcall (gethash ,key ,hash-table) ,val-name))))))

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
