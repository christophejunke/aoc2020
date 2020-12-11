(in-package :aoc2020)

(defun aoc-package (day)
  (find-package (format nil "AOC2020.~2,'0d" day)))

(defun aoc-packages ()
  (loop
    for day from 25 downto 0
    collect (aoc-package day)))

(defmacro define-test (name &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (eval-when (:compile-toplevel :execute)
       (setf (get ',name 'test) t)
       (setf (get ',name 'dirty) t))))

(defun unit-test (i s f)
  (list i s (nth-value 1 (ignore-errors (funcall f)))))

(unless *kernel*
  (setf *kernel*
        (make-kernel 25 :name "aoc2020"
                        :bindings nil)))

(defun format-result (res)
  (with-standard-io-syntax
    (let ((*print-right-margin* most-positive-fixnum))
      (loop
        for (s e) in (mapcar #'cdr (sort res #'> :key #'car))
        for n = (if (string= s "TEST") "" (format nil " > ~a" s))
        for p = (package-name (symbol-package s))
        collect n into snames
        collect p into pnames
        collect e into errors
        maximize (length n) into max-s
        maximize (length p) into max-p
        finally
           (flet ((fmt (s p e)
                    (format *trace-output*
                            "~&> ~va~va~:[~; > ~:*~a~]~%"
                            max-p p
                            max-s s
                            e)))
             (map () #'fmt snames pnames errors))))))

(defun test-all-in-packages (packages &optional (force nil))
  (let ((packages (ensure-list packages))
        (channel (make-channel))
        (res)
        (pass t)
        (submitted 0))
    (time
     (progn
       (dolist (package packages pass)
         (do-external-symbols (symbol package)
           (when-let ((function (and (get symbol 'test)
                                     (fboundp symbol)
                                     (symbol-function symbol))))
             (when (or force (get symbol 'dirty))
               (submit-task channel
                            #'unit-test
                            (incf submitted)
                            symbol
                            function)))))
       (do-fast-receives (result channel submitted)
         (push result res)
         (destructuring-bind (i s e) result
           (declare (ignore i))
           (if e
               (setf pass nil)
               (setf (get s 'dirty) nil))))))
    (prog1 pass
      (format-result res))))

(defvar *test-lock* (bt:make-lock "aoc-tests"))

(defun test-all (&optional (force nil))
  (bt:with-lock-held (*test-lock*)
    (test-all-in-packages (aoc-packages) force)))

