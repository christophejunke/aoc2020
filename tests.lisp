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

(defun symname (symbol)
  (if (string= symbol "TEST")
      ""
      (format nil " > ~a" symbol)))

(defun format-result (res)
  (with-standard-io-syntax
    (let ((*print-right-margin* most-positive-fixnum))
      (loop
        for (i s e) in (sort res #'> :key #'car)
        for sn = (symname s)
        for pn = (package-name (symbol-package s))
        collect sn into snames
        collect pn into pnames
        collect e into errors
        maximize (length (symname s))
        into max-s
        maximize (length pn)
        into max-p
        finally
           (loop
             for s in snames
             for p in pnames
             for e in errors
             do (format *trace-output*
                        "~&> ~va~va~:[~; > ~:*~a~]~%"
                        max-p p
                        max-s s
                        e))))))

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

