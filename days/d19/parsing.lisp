(in-package :aoc2020.19)

;;; ============================================================
;;; GADT for parsed input
;;; ============================================================

(deftype non-terminal () '(or fixnum symbol))

(adt:defdata rule-body
  (rule.expr cons)
  (rule.term character))

(adt:defdata parsed-rule
  (rule.production non-terminal rule-body))

(defun production (lhs &rest rhs)
  (rule.production lhs (rule.expr rhs)))


;;; ============================================================
;;; PARSING INPUT
;;; ============================================================

(defclass input ()
  ((grammar :initarg :grammar :reader grammar)
   (source :initarg :source :reader source)
   (entries :initarg :entries :reader entries)))

(defun lhs (p) (adt:with-data (rule.production lhs _) p lhs))
(defun rhs (p) (adt:with-data (rule.production _ rhs) p rhs))

(defun make-input (&key rules entries source)
  (let* ((rules (sort (copy-seq rules) #'< :key #'lhs))
         (start (find 0 rules :key #'lhs)))
    (assert start)
    (make-instance 'input
                   :grammar (make-grammar :rules rules :start start)
                   :source source
                   :entries entries)))

(defun scanner ()
  (load-time-value
   (create-scanner
    '(:sequence #\" (:register (:regex "\\w+")) #\"))))

(defun parse-rule (line &aux alternatives)
  (destructuring-bind (line body) (split ": " line)
    (let ((line (parse-integer line)))
      (if-let (terminal (scanner-bind ("\"%c\"" c) body
                          (rule.term c)))
        (push (rule.production line terminal) alternatives)
        (dolist (alt (split '(:group " | ") body))
          (let ((terms (mapcar #'parse-integer (split #\space alt))))
            (push (rule.production line (rule.expr terms)) alternatives))))
      alternatives)))

(defun input (name)
  (with-input (in name)
    (make-input
     :source (pathname in)
     :rules (let ((rules (list)))
              (do-input-lines (line in (error "no entry"))
                (when (= (length line) 0)
                  (return rules))
                (dolist (alt (parse-rule line))
                  (push alt rules))))
     :entries (map-input in :type 'list))))
