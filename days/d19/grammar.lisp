(in-package :aoc2020.19)

;;; ============================================================
;;; Grammar base interface
;;; ============================================================

(defclass grammar ()
  ((rules :initarg :rules :reader rules)
   (start :initarg :start :reader start)))

(defvar *grammar-class*)

(defun grammar-class (&optional prototype)
  (cond
    ((and (boundp '*grammar-class*) *grammar-class*))
    (prototype (class-of prototype))
    (t (load-time-value (find-class 'grammar)))))

(defun make-grammar (&key rules start)
  (make-instance (grammar-class) :rules rules :start start))

(defgeneric updated-grammar-from (original new)
  (:method-combination progn)
  (:method progn (n o)))

(defun update-grammar (grammar &key (start nil sp) (rules nil rp))
  (let ((*grammar-class* (grammar-class grammar)))
    (let ((new (make-grammar :start (if sp start (start grammar))
                             :rules (if rp rules (rules grammar)))))
      (prog1 new
        (updated-grammar-from grammar new)))))
