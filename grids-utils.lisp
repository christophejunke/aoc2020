(in-package :aoc2020)

(defun ignorable-symbol-p (s)
  (or (null s)
      (let ((s (string s)))
        (or (= 0 (length s))
            (char= #\_ (char s 0))))))

(defun lambda-list-array-dimension (args)
  (multiple-value-bind (req opt rest key aokp aux kp)
      (alexandria:parse-ordinary-lambda-list args)
    (declare (ignore aux))
    (and (not opt)
         (not rest)
         (not key)
         (not aokp)
         (not kp)
         (length req))))

(defun ignorable-variables (vars)
  (loop
    for v in vars
    for i = (ignorable-symbol-p v)
    for w = (if i (gensym "_") v)
    collect w into vars
    when i
    collect w into ignored
    finally (return (values vars ignored))))

