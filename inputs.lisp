(in-package :aoc2020)

(defmacro with-input ((stream name) &body body)
  `(with-open-file (,stream (aoc2020.fetch:fetch-input ,name nil))
     ,@body))

(defmacro do-input-lines ((line name &optional result) &body body)
  (with-gensyms (stream)
    `(with-input (,stream ,name)
       (loop :for ,line := (read-line ,stream nil nil)
             :while ,line
             :do (progn ,@body)
             :finally (return ,result)))))

(defun fold-input-lines (input function &optional accumulator)
  (do-input-lines (line input accumulator)
    (setf accumulator (funcall function line accumulator))))

(defun slurp-line (input)
  (with-input (s input)
    (read-line s)))

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

